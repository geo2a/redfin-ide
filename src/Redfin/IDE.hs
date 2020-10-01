{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Redfin.IDE
  ( ideWidget
  , mkIDE
  ) where

import           Colog                         (pattern D, pattern E,
                                                HasLog (..), pattern I,
                                                LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                hiding (id)
import qualified Concur.Replica.DOM.Events     as P
import           Control.Applicative           (empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative      as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                    as A
import           Data.Functor                  (void)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Text
import qualified Data.Text.Read                as Text
import           Prelude                       hiding (div, log, lookup, span)
import           Replica.VDOM.Render           as Render
import           Text.Read                     (readEither)

import           ISA.Assembly                  (Script, assemble)
import           ISA.Backend.Symbolic.List.Run (runModel)
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace      hiding (htmlTrace)

import           Redfin.IDE.State
import           Redfin.IDE.Trace              (htmlTrace)
import           Redfin.IDE.Types

import qualified ISA.Example.Add               as Example
import qualified ISA.Example.Sum               as ExampleSum

import qualified Debug.Trace                   as Debugger

topWidget :: App a
topWidget = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [ examplesWidget
        , stepsWidget (Right (_stepsVal ?ide))
        ]
  when (steps' /= (_stepsVal ?ide)) $
      liftIO . atomically $ putTMVar (_steps ?ide) steps'
  topWidget

-- | Specify the number of symbolic execution steps
stepsWidget :: Either Text Steps -> Widget HTML Steps
stepsWidget s = do
  let msg = either (const "") (Text.pack . show) s
  txt <- targetValue . target <$>
         div [classList [("stepsWidget", True)]]
             [ text ("Symbolic execution steps: ")
             , input [ placeholder msg
                     , value ""
                     , onChange, autofocus True]
             , either (const $ text "Error: invalid input") (const empty) s
             ]
  case (readEither . Text.unpack $ txt) of
    Left err -> stepsWidget (Left (Text.pack err))
    Right x  -> pure x

-- | Display the assembly source code of the program
sourceWidget :: App a
sourceWidget =
  div [classList [ ("pane", True)
                 , ("leftpane", True)
                 ]
      ]
      [ol [classList [("sourceCode", True)]] (map (li [] . (:[]) . text) src)]
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) $ assemble (_source ?ide)

-- | Display the symbolic execution trace
--   Clicking on a node will display the node's
--   state in the state widget on the very right
traceWidget :: App a
traceWidget = widget
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO $ readTVarIO (_trace ?ide)
      div [classList [ ("pane", True)
                     , ("middlepane", True)
                     ]
          ]
          [pre [] [htmlTrace trace]]

stateWidget :: App a
stateWidget = do
  widget 0
  where
    widget :: NodeId -> Widget HTML a
    widget !n = do
      log D $ "displaying state for node " <> Text.pack (show n)
      trace <- liftIO $ readTVarIO (_trace ?ide)

      ev <- div [classList [ ("pane", True)
                           , ("rightpane", True)
                           ]
                ]
                [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
                , displayContext (lookup n trace)
                , Right <$> (liftIO . atomically $ readTQueue (_activeNodeQueue ?ide))
                ]
      case ev of
        Left _   -> widget n
        Right n' -> widget n'

-- | Cook the initial IDE state
mkIDE :: Example -> LogAction (Widget HTML) Message -> IO IDEState
mkIDE ex logger = do
  nodeIdQueue <- liftIO newTQueueIO
  atomically $ writeTQueue nodeIdQueue 0
  stepsVar  <- liftIO $ newTMVarIO 0
  exampleVar <- liftIO $ newTMVarIO ex

  case ex of
    ExampleAdd   -> do
      traceVar  <- liftIO $ newTVarIO (Example.symexecTrace 0)
      pure (IDEState traceVar stepsVar 0 nodeIdQueue
             exampleVar ex
             Example.addLowLevel
             Example.symexecTrace
             logger)
    ExampleSum   -> do
      traceVar  <- liftIO $ newTVarIO (runModel 0 ExampleSum.initContext)
      pure (IDEState traceVar stepsVar 0 nodeIdQueue
             exampleVar ex
             ExampleSum.sumArrayLowLevel
             (\s -> runModel s ExampleSum.initContext)
             logger)
    -- ExampleGCD   ->
    -- ExampleMotor ->

swapExample :: IDEState -> Example -> IDEState
swapExample ide = \case
  ExampleAdd -> ide { _source = Example.addLowLevel
                    , _runSymExec = Example.symexecTrace
                    , _activeExampleVal = ExampleAdd
                    , _stepsVal = 0
                    }
  ExampleSum -> ide { _source = ExampleSum.sumArrayLowLevel
                    , _runSymExec = \s -> runModel s ExampleSum.initContext
                    , _activeExampleVal = ExampleSum
                    , _stepsVal = 0
                    }

examplesWidget :: App a
examplesWidget = do
  log I "Example widget initialised"
  e <- ul [classList [("examplesWidget", True)]]
          [ exampleButton ExampleAdd
          , exampleButton ExampleSum
          -- , li [] [span [] [button [ExampleGCD <$ onClick] [text "GCD"]]]
          -- , li [] [span [] [button [ExampleMotor <$ onClick] [text "Motor"]]]
          ]
  liftIO . atomically $ putTMVar (_activeExample ?ide) e
  examplesWidget
  where exampleButton ex =
          li [] [a [ classList [ ("exampleButton", True)
                               , ("activeExample", ex == _activeExampleVal ?ide)]
                   , ex <$ onClick]
                   [text (Text.pack $ show ex)]]

data Event = Proceed
           | ExampleChanged Example
           | StepsChanged Steps
           deriving Show

elimEvent :: Event -> App IDEState
elimEvent = \case
  Proceed -> pure ?ide
  ExampleChanged ex -> do
    log D $ "Example changed to " <> Text.pack (show ex)
    let ide' = swapExample ?ide ex
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') (_runSymExec ide' 0)
      cleanupQueues ide'
    pure ide'
  StepsChanged steps -> do
    log D $ "Steps changed to " <> Text.pack (show steps)
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ?ide) (_runSymExec ?ide steps)
      cleanupQueues ?ide
    log D $ "Trace regenerated with " <> Text.pack (show steps) <> " steps"
    pure (?ide {_stepsVal = steps})
  where cleanupQueues :: IDEState -> STM ()
        cleanupQueues ide =
          flushTQueue (_activeNodeQueue ide) *>
          writeTQueue (_activeNodeQueue ide) 0

ideWidget :: App a
ideWidget = do
  event <-  MultiAlternative.orr
      [ Proceed <$ topWidget
      , Proceed <$ sourceWidget
      , Proceed <$ traceWidget
      , Proceed <$ stateWidget

      , ExampleChanged <$> (liftIO . atomically . takeTMVar $ _activeExample ?ide)
      , StepsChanged <$> (liftIO . atomically . takeTMVar $ _steps ?ide)
      ]
  ide' <- elimEvent event
  let ?ide = ide' in
    ideWidget
