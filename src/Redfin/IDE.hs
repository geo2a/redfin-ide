{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Redfin.IDE
  ( ideWidget
  , mkIDE
  ) where

import           Colog                              (pattern D, pattern E,
                                                     HasLog (..), pattern I,
                                                     LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                     hiding (id)
import qualified Concur.Replica.DOM.Events          as P
import           Control.Applicative                (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative           as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                         as A
import           Data.Either                        (rights)
import           Data.Functor                       (void)
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Lazy.Builder             as Text
import qualified Data.Text.Read                     as Text
import           Prelude                            hiding (div, log, lookup,
                                                     span)
import           Replica.VDOM.Render                as Render
import           Text.Read                          (readEither)

import           ISA.Backend.Symbolic.List.QueryRun (runModel)
import qualified ISA.Example.Add                    as ExampleAdd
import qualified ISA.Example.Sum                    as ExampleSum
import           ISA.Types                          hiding (not)
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace           hiding (htmlTrace)

import           Redfin.IDE.State
import           Redfin.IDE.Trace                   (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.InitialState
import           Redfin.IDE.Widget.Source
import           Redfin.IDE.Widget.State
import           Redfin.IDE.Widget.Top
import           Redfin.IDE.Widget.Top.Examples
import           Redfin.IDE.Widget.Trace

import qualified Debug.Trace                        as Debugger

-- | Cook the initial IDE state
mkIDE :: Example -> LogAction (Widget HTML) Message -> IO IDEState
mkIDE ex logger =
  (flip swapExample) ex <$> (emptyIDE logger)

leftPane :: App a
leftPane = do
  newInitState <-
    div [classList [ ("pane", True)
                   , ("leftpane", True)
                   ]
        ]
        [ sourceWidget
        , initStateWidget (_activeInitStateVal ?ide)
        ]
  when (newInitState /= (_activeInitStateVal ?ide)) $
    liftIO . atomically $ putTMVar (_activeInitState ?ide) newInitState
  leftPane

data Event = Proceed
           | ExampleChanged Example
           | StepsChanged Steps
           | InitStateChanged Context
           | SolveButtonPressed
           | TraceDisplayToggled
           deriving Show

elimEvent :: Event -> App IDEState
elimEvent = \case
  Proceed -> pure ?ide
  ExampleChanged ex -> do
    log D $ "Example changed to " <> Text.pack (show ex)
    let ide' = swapExample ?ide ex
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') emptyTrace
      cleanupQueues ide'
    pure ide'
  StepsChanged steps -> do
    log D $ "Steps changed to " <> Text.pack (show steps)
    trace <- liftIO $ _runSymExec ?ide steps (_activeInitStateVal ?ide)
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ?ide) trace
      cleanupQueues ?ide
    log D $ "Trace regenerated with " <> Text.pack (show steps) <> " steps"
    pure (?ide {_stepsVal = steps})
  InitStateChanged ctx -> do
    log D $ "Init state changed"
    let ide' = ?ide {_activeInitStateVal = ctx, _stepsVal = 0}
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') emptyTrace
      cleanupQueues ide'
    pure ide'
  SolveButtonPressed -> do
    log D $ "Solve button pressed"
    trace <- liftIO . atomically $ readTVar (_trace ?ide)
    solvedTrace <- liftIO $ (_solve ?ide) trace
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ?ide) solvedTrace
      cleanupQueues ?ide
    log D $ "Trace solved"
    pure ?ide
  TraceDisplayToggled -> do
    log D $ "Trace display unreachable checkbox toggled"
    let display' = not (_displayUnreachableVal ?ide)
    pure $ ?ide {_displayUnreachableVal = display'}
  where cleanupQueues :: IDEState -> STM ()
        cleanupQueues ide =
          flushTQueue (_activeNodeQueue ide) *>
          writeTQueue (_activeNodeQueue ide) 0

ideWidget :: App a
ideWidget = do
  event <- MultiAlternative.orr
      [ Proceed <$ div [classList [("grid-container", True)]]
                       [ topPane
                       , leftPane
                       , traceWidget
                       , stateWidget]

      , ExampleChanged <$> (liftIO . atomically . takeTMVar $ _activeExample ?ide)
      , StepsChanged <$> (liftIO . atomically . takeTMVar $ _steps ?ide)
      , InitStateChanged <$> (liftIO . atomically . takeTMVar $ _activeInitState ?ide)
      , SolveButtonPressed <$ (liftIO . atomically . takeTMVar $ _solving ?ide)
      , TraceDisplayToggled <$ (liftIO . atomically . takeTMVar $ _displayUnreachable ?ide)
      ]
  ide' <- elimEvent event
  let ?ide = ide' in
    ideWidget
