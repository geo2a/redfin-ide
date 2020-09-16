{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Redfin.IDE
  ( ideWidget
  , mkIDE
  ) where

import           Colog.Core                    (LogAction (..), logStringStdout,
                                                (<&))
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
import           Prelude                       hiding (div, lookup, span)
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

topWidget :: Steps -> App a
topWidget steps logger ide = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [Old <$> examplesWidget logger ide, New <$> stepsWidget steps]
  case steps' of
    Old s -> topWidget s logger ide
    New s' -> do
      liftIO . atomically $ putTMVar (_steps ide) s' *>
                            writeTVar (_trace ide) (_runSymExec ide s')
      topWidget s' logger ide


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
sourceWidget logger ide =
  div [classList [ ("pane", True)
                 , ("leftpane", True)
                 ]
      ]
      [ol [classList [("sourceCode", True)]] (map (li [] . (:[]) . text) src)]
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) $ assemble (_source ide)

-- | Display the symbolic execution trace
--   Clicking on a node will display the node's
--   state in the state widget on the very right
traceWidget :: Steps
            -> App a
traceWidget steps logger ide =
  widget
  -- widget <|> (liftIO . atomically $ takeTMVar (_steps ide)) >>=
  --   \steps' -> traceWidget steps' ide
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO $ readTVarIO (_trace ide)
      div [classList [ ("pane", True)
                     , ("middlepane", True)
                     ]
          ]
          [pre [] [htmlTrace trace logger ide]]

-- | Display the state of the selected node
--   TODO: Maybe invalidate the state widget after changing the amount of steps,
--   since nodes can have different ids in different partial traces
stateWidget :: NodeId -> App a
stateWidget n logger ide =
  widget <|> (liftIO . atomically $ takeTMVar (_activeNode ide)) >>=
    \n -> stateWidget n logger ide
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO $ readTVarIO (_trace ide)
      div [classList [ ("pane", True)
                     , ("rightpane", True)
                     ]
          ]
          [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
          , displayContext (lookup n trace)
          ]

-- | Cook the initial IDE state
mkIDE :: Example -> IO IDEState
mkIDE ex = do
  nodeIdVar <- liftIO $ newTMVarIO 0
  stepsVar  <- liftIO $ newTMVarIO 0
  exampleVar <- liftIO $ newTMVarIO ex
  case ex of
    ExampleAdd   -> do
      traceVar  <- liftIO $ newTVarIO (Example.symexecTrace 0)
      pure (IDEState traceVar stepsVar nodeIdVar
             exampleVar
             Example.addLowLevel
             Example.symexecTrace)
    ExampleSum   -> do
      traceVar  <- liftIO $ newTVarIO (runModel 0 ExampleSum.initContext)
      pure (IDEState traceVar stepsVar nodeIdVar
             exampleVar
             ExampleSum.sumArrayLowLevel
             (\s -> runModel s ExampleSum.initContext))
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
examplesWidget logger ide = do
  e <- ul [] [ li [] [span [] [button [ExampleAdd <$ onClick] [text "Add"]]]
             , li [] [span [] [button [ExampleSum <$ onClick] [text "Sum"]]]
             -- , li [] [span [] [button [ExampleGCD <$ onClick] [text "GCD"]]]
             -- , li [] [span [] [button [ExampleMotor <$ onClick] [text "Motor"]]]
             ]
  liftIO . atomically $ putTMVar (_activeExample ide) e
  examplesWidget logger ide
  -- pure e

data Event = Proceed
           | ExampleChanged Example
           | StepsChanged Steps
           deriving Show

elimEvent :: IDEState -> Event -> STM IDEState
elimEvent ide = \case
  Proceed -> pure ide
  ExampleChanged ex -> do
    let ide' = swapExample ide ex
    writeTVar (_trace ide') (_runSymExec ide' 0)
    pure ide'
  StepsChanged steps -> do
    -- putTMVar (_steps ide) steps
    -- writeTVar (_trace ide) (_runSymExec ide steps)
    pure ide

ideWidget :: App a
ideWidget logger ide =
  -- e <- examplesWidget
  -- e <- pure ex <|> (liftIO . atomically . takeTMVar $ _activeExample initialState)
  ideWidget logger =<< liftIO . atomically . elimEvent ide =<<
    MultiAlternative.orr
      -- . map ($ initialState) $
      [ Proceed <$ topWidget 0 logger ide
      , Proceed <$ sourceWidget logger ide
      , Proceed <$ traceWidget 0 logger ide
      , Proceed <$ stateWidget 0 logger ide

      , ExampleChanged <$> (liftIO . atomically . takeTMVar $ _activeExample ide)
      , StepsChanged <$> (liftIO . atomically . takeTMVar $ _steps ide)
      -- , Left <$> (liftIO . atomically . takeTMVar $ _steps ide)
      ]
