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
import qualified ISA.Example.Add                    as Example
import qualified ISA.Example.Sum                    as ExampleSum
import           ISA.Types
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace           hiding (htmlTrace)

import           Redfin.IDE.State
import           Redfin.IDE.Trace                   (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.Examples
import           Redfin.IDE.Widget.InitialState
import           Redfin.IDE.Widget.Source
import           Redfin.IDE.Widget.State
import           Redfin.IDE.Widget.Steps
import           Redfin.IDE.Widget.Trace

import qualified Debug.Trace                        as Debugger

topPane :: App a
topPane = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [ examplesWidget
        , stepsWidget (Right (_stepsVal ?ide))
        , smtWidget
        ]
  when (steps' /= (_stepsVal ?ide)) $
      liftIO . atomically $ putTMVar (_steps ?ide) steps'
  topPane

smtWidget :: App a
smtWidget = do
  e <- div [classList [ ("box", True)
                      , ("SMTWidget", True)
                      ]
           ]
        [ True <$ button [onClick] [text "Solve"]
        ]
  when e $
    log I "Solve button clicked"
  smtWidget

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

emptyCtx :: Context
emptyCtx = MkContext Map.empty (SConst (CBool True)) []

-- | Cook the initial IDE state
mkIDE :: Example -> LogAction (Widget HTML) Message -> IO IDEState
mkIDE ex logger = do
  nodeIdQueue <- liftIO newTQueueIO
  atomically $ writeTQueue nodeIdQueue 0
  stepsVar  <- liftIO $ newTMVarIO 0
  exampleVar <- liftIO $ newTMVarIO ex
  ctxVar <- liftIO $ newTMVarIO emptyCtx

  case ex of
    ExampleAdd   -> do
      trace <- liftIO $ runModel 0 Example.initCtx
      traceVar  <- liftIO $ newTVarIO trace
      pure (IDEState traceVar stepsVar 0 nodeIdQueue
             exampleVar ex
             ctxVar emptyCtx
             Example.addLowLevel
             runModel
             logger)
    ExampleSum   -> do
      trace <- liftIO $ runModel 0 ExampleSum.initContext
      traceVar  <- liftIO $ newTVarIO trace
      pure (IDEState traceVar stepsVar 0 nodeIdQueue
             exampleVar ex
             ctxVar emptyCtx
             ExampleSum.sumArrayLowLevel
             runModel
             logger)
    -- ExampleGCD   ->
    -- ExampleMotor ->

data Event = Proceed
           | ExampleChanged Example
           | StepsChanged Steps
           | InitStateChanged Context
           deriving Show

elimEvent :: Event -> App IDEState
elimEvent = \case
  Proceed -> pure ?ide
  ExampleChanged ex -> do
    log D $ "Example changed to " <> Text.pack (show ex)
    let ide' = swapExample ?ide ex
    trace <- liftIO $ _runSymExec ide' 0 (_activeInitStateVal ide')
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') trace
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
    trace <- liftIO $ _runSymExec ide' 0 (_activeInitStateVal ide')
    oldQueue <- liftIO . atomically $ do
      writeTVar (_trace ide') trace
      cleanupQueues ide'
    pure ide'
  where cleanupQueues :: IDEState -> STM ()
        cleanupQueues ide =
          flushTQueue (_activeNodeQueue ide) *>
          writeTQueue (_activeNodeQueue ide) 0

ideWidget :: App a
ideWidget = do
  event <- MultiAlternative.orr
      [ Proceed <$ topPane
      , Proceed <$ leftPane
      , Proceed <$ traceWidget
      , Proceed <$ stateWidget

      , ExampleChanged <$> (liftIO . atomically . takeTMVar $ _activeExample ?ide)
      , StepsChanged <$> (liftIO . atomically . takeTMVar $ _steps ?ide)
      , InitStateChanged <$> (liftIO . atomically . takeTMVar $ _activeInitState ?ide)
      ]
  ide' <- elimEvent event
  let ?ide = ide' in
    ideWidget
