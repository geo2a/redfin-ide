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

import           Colog                         (pattern D, HasLog (..),
                                                pattern I, LogAction, Message)
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

topWidget :: Steps -> App a
topWidget steps = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [Old <$> examplesWidget, New <$> stepsWidget steps]
  case steps' of
    Old s -> topWidget s
    New s' -> do
      liftIO . atomically $ putTMVar (_steps ?ide) s'
      topWidget s'

-- | Specify the number of symbolic execution steps
stepsWidget :: Steps -> Widget HTML Steps
stepsWidget s =
  read . Text.unpack . targetValue . target <$>
    div []
      [ text ("Symbolic execution steps: " <> Text.pack (show s))
      , input [placeholder "Change", onChange, autofocus True]
      ]

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
traceWidget :: Steps
            -> App a
traceWidget steps = widget
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO . atomically $ readTMVar (_trace ?ide)
      div [classList [ ("pane", True)
                     , ("middlepane", True)
                     ]
          ]
          [pre [] [htmlTrace trace]]

-- | Display the state of the selected node
--   TODO: Maybe invalidate the state widget after changing the amount of steps,
--   since nodes can have different ids in different partial traces
-- stateWidget :: App a
-- stateWidget = do
--   widget 0
--   where
--     widget :: NodeId -> Widget HTML a
--     widget n = do
--       trace <- liftIO $ readTVarIO (_trace ?ide)
--       let displayed =
--             div [classList [ ("pane", True)
--                            , ("rightpane", True)
--                            ]
--                 ]
--                 [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
--                 , displayContext (lookup n trace)
--                 ]
--           fetched = liftIO . atomically $ readTQueue (_activeNodeQueue ?ide)
--             -- xs <- flushTQueue (_activeNodeQueue ?ide)
--             -- case xs of
--             --   []    -> retry
--             --   (x:_) -> pure x
--             -- readTQueue (_activeNodeQueue ?ide)
--       Old <$> displayed <|> New <$> fetched >>=
--         \case Old _  -> Debugger.trace "I'm old!!" $ widget n
--               New n' -> do
--                 Debugger.trace "I'm new!!" $ widget n'
stateWidget :: App a
stateWidget = do
  widget 0
  where
    widget :: NodeId -> Widget HTML a
    widget !n = do
      log D $ "displaying state for node " <> Text.pack (show n)
      trace <- liftIO . atomically $ readTMVar (_trace ?ide)

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
      -- Old <$> displayed <|> New <$> fetched >>=
      --   \case Old _  -> Debugger.trace "I'm old!!" $ widget n
      --         New n' -> do
      --           Debugger.trace "I'm new!!" $ widget n'



-- | Cook the initial IDE state
mkIDE :: Example -> LogAction (Widget HTML) Message -> IO IDEState
mkIDE ex logger = do
  nodeIdQueue <- liftIO newTQueueIO
  atomically $ writeTQueue nodeIdQueue 0
  stepsVar  <- liftIO $ newTMVarIO 0
  exampleVar <- liftIO $ newTMVarIO ex

  case ex of
    ExampleAdd   -> do
      traceVar  <- liftIO $ newTMVarIO (Example.symexecTrace 0)
      pure (IDEState traceVar stepsVar nodeIdQueue
             exampleVar
             Example.addLowLevel
             Example.symexecTrace
             logger)
    ExampleSum   -> do
      traceVar  <- liftIO $ newTMVarIO (runModel 0 ExampleSum.initContext)
      pure (IDEState traceVar stepsVar nodeIdQueue
             exampleVar
             ExampleSum.sumArrayLowLevel
             (\s -> runModel s ExampleSum.initContext)
             logger)
    -- ExampleGCD   ->
    -- ExampleMotor ->

swapExample :: IDEState -> Example -> IDEState
swapExample ide = \case
  ExampleAdd -> ide { _source = Example.addLowLevel
                    , _runSymExec = Example.symexecTrace
                    }
  ExampleSum -> ide { _source = ExampleSum.sumArrayLowLevel
                    , _runSymExec = \s -> runModel s ExampleSum.initContext
                    }

examplesWidget :: App a
examplesWidget = do
  e <- ul [] [ exampleButton ExampleAdd
             , exampleButton ExampleSum
             -- , li [] [span [] [button [ExampleGCD <$ onClick] [text "GCD"]]]
             -- , li [] [span [] [button [ExampleMotor <$ onClick] [text "Motor"]]]
             ]
  liftIO . atomically $ putTMVar (_activeExample ?ide) e
  examplesWidget
  where exampleButton ex =
          li [] [button [ex <$ onClick] [text (Text.pack $ show ex)]]
  -- pure e

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
    liftIO . atomically $ do
      _ <- flushTQueue (_activeNodeQueue ide')
      writeTQueue (_activeNodeQueue ide') 0
      swapTMVar (_trace ide') (_runSymExec ide' 0)
    pure ide'
  StepsChanged !steps -> do
    log D $ "Steps changed to " <> Text.pack (show steps)
    -- putTMVar (_steps ide) steps
    liftIO . atomically $ do
      _ <- flushTQueue (_activeNodeQueue ?ide)
      writeTQueue (_activeNodeQueue ?ide) 0
      swapTMVar (_trace ?ide) (_runSymExec ?ide steps)
    log D $ "Trace regenerated with " <> Text.pack (show steps) <> " steps"
    pure ?ide

ideWidget :: App a
ideWidget =
  -- e <- examplesWidget
  -- e <- pure ex <|> (liftIO . atomically . takeTMVar $ _activeExample initialState)
  (\ide' -> let ?ide = ide' in ideWidget) =<< elimEvent =<<
    MultiAlternative.orr
      -- . map ($ initialState) $
      [ Proceed <$ topWidget 0
      , Proceed <$ sourceWidget
      , Proceed <$ traceWidget 0
      , Proceed <$ stateWidget

      , ExampleChanged <$> (liftIO . atomically . takeTMVar $ _activeExample ?ide)
      , StepsChanged <$> (liftIO . atomically . takeTMVar $ _steps ?ide)
      -- , Left <$> (liftIO . atomically . takeTMVar $ _steps ide)
      ]
