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

import           Colog                           (HasLog (..), LogAction,
                                                  Message, pattern D, pattern E,
                                                  pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                  hiding (id)
import qualified Concur.Replica.DOM.Events       as P
import           Control.Applicative             (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative        as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                      as A
import           Data.Either                     (rights)
import           Data.Functor                    (void)
import           Data.Int                        (Int32)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
import qualified Data.Text.Read                  as Text
import           Prelude                         hiding (div, log, lookup, span)
import           Replica.VDOM.Render             as Render
import           Text.Read                       (readEither)

import           ISA.Backend.Symbolic.Zipper     hiding (_trace)
import qualified ISA.Backend.Symbolic.Zipper.Run as ISA (runModel)
import qualified ISA.Example.Add                 as ExampleAdd
import qualified ISA.Example.Sum                 as ExampleSum
import           ISA.Types
import           ISA.Types.Context               hiding (Context)
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic

import           Redfin.IDE.State
import           Redfin.IDE.Trace                (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.InitialState
import           Redfin.IDE.Widget.Source
import           Redfin.IDE.Widget.State
import           Redfin.IDE.Widget.Top
import           Redfin.IDE.Widget.Top.Examples
import           Redfin.IDE.Widget.Trace

import qualified Debug.Trace                     as Debugger

-- | Cook the initial IDE state
mkIDE :: Example -> IO IDEState
mkIDE ex =
  (flip swapExample) ex =<< emptyIDE

leftPane :: App [(CAddress, Instruction (Data Int32))]
leftPane = do
  section [classList [ ("pane", True)
                   , ("leftpane", True)
                   ]]
    [ div [classList [ ("leftpane-contents", True)]]
          [ sourceWidget ""
          , initStateWidget (_activeInitStateVal ?ide)
          ]
    ]

data Event = Proceed
           | SourceChanged [(CAddress, Instruction (Data Int32))]
           | SaveLoaded IDEState
           | ExampleChanged Example
           | StepsChanged Steps
           | TimeoutChanged Int
           | InitStateChanged Context
           | SolveButtonPressed
           | TraceDisplayToggled

elimEvent :: Event -> App IDEState
elimEvent = \case
  Proceed -> pure ?ide
  SourceChanged src -> pure $ ?ide { _source = src }
  SaveLoaded ide -> pure ide
  ExampleChanged ex -> do
    log D $ "Example changed to " <> Text.pack (show ex)
    ide' <- liftIO $ swapExample ?ide ex
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') emptyTrace
      cleanupQueues ide'
    pure ide'
  StepsChanged steps -> do
    log D $ "Steps changed to " <> Text.pack (show steps)
    trace <- liftIO $ ISA.runModel steps (_activeInitStateVal ?ide)
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ?ide) trace
      cleanupQueues ?ide
    log D $ "Trace regenerated with " <> Text.pack (show steps) <> " steps"
    -- log I $ "Stats: " <> (Text.pack . show $ stats)
    pure (?ide {_stepsVal = steps})
  TimeoutChanged timeout -> do
    log D $ "Timeout changed to " <> Text.pack (show timeout)
    pure (?ide {_timeoutVal = timeout})
  InitStateChanged ctx -> do
    log D $ "Init state changed"
    let ide' = ?ide {_activeInitStateVal = ctx}
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') emptyTrace
      cleanupQueues ide'
    pure ide'
  SolveButtonPressed -> do
    log D $ "Solve button pressed"
    trace <- liftIO . atomically $ readTVar (_trace ?ide)
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ?ide) trace
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
  event <- div [classList [("grid-container", True)]]
      [ SaveLoaded <$> topPane
      , SourceChanged <$> leftPane
      , Proceed <$ MultiAlternative.orr
                       [ traceWidget
                       , stateWidget]

      , ExampleChanged <$> (liftIO . atomically . takeTMVar $ _activeExample ?ide)
      , StepsChanged <$> (liftIO . atomically . takeTMVar $ _steps ?ide)
      , TimeoutChanged <$> (liftIO . atomically . takeTMVar $ _timeout ?ide)
      , InitStateChanged <$> (liftIO . atomically . takeTMVar $ _activeInitState ?ide)
      , SolveButtonPressed <$ (liftIO . atomically . takeTMVar $ _solving ?ide)
      , TraceDisplayToggled <$ (liftIO . atomically . takeTMVar $ _displayUnreachable ?ide)
      ]
  ide' <- elimEvent event
  let ?ide = ide' in
    ideWidget
