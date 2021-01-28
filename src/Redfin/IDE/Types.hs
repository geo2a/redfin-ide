{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Redfin.IDE.Types where

import           Colog                       (pattern D, HasLog (..), pattern I,
                                              LogAction (..), Message, Severity,
                                              WithLog, richMessageAction)
import           Colog.Message               (Msg (..))
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT (..))

import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica              hiding (id)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import           GHC.Stack                   (HasCallStack, callStack,
                                              withFrozenCallStack)

import           ISA.Assembly
import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace

-- | Either-like datatype for tracking new/old content of tags
data Contents a b = Old a
                  | New b
                  deriving (Show, Eq, Ord)

-- | A pair of a 'ISA.Types.Key' and a value
--   Eq and Ord instances only consider the key
data WithKey a = MkWithKey Key a
  deriving Show

instance Eq (WithKey a) where
  (MkWithKey k1 _) == (MkWithKey k2 _) = k1 == k2

instance Ord (WithKey a) where
  (MkWithKey k1 _) <= (MkWithKey k2 _) = k1 <= k2

type Steps = Int

data Example = None
             | Add
             | Sum
             -- | ExampleGCD
             | MotorLoop
             deriving (Show, Eq)

data IDEState =
  IDEState { _trace                 :: TVar (Trace Context)
           , _displayUnreachable    :: TMVar Bool
           , _displayUnreachableVal :: Bool

           , _steps                 :: TMVar Steps
           , _stepsVal              :: Steps

           , _activeNodeQueue       :: TQueue NodeId

           , _activeExample         :: TMVar Example
           , _activeExampleVal      :: Example

           , _activeInitState       :: TMVar Context
           , _activeInitStateVal    :: Context

           , _source                :: Script
           , _runSymExec            :: Steps -> Context -> IO (Trace Context)

           , _solving               :: TMVar ()
           , _solve                 :: Trace Context -> IO (Trace Context)


           , _logger                :: LogAction (Widget HTML) Message
           }

type App a = (HasCallStack, ?ide :: IDEState) => Widget HTML a

emptyCtx :: Context
emptyCtx = MkContext Map.empty (SConst (CBool True)) [] Nothing

emptyTrace :: Trace Context
emptyTrace = mkTrace (Node 0 emptyCtx) []

emptyIDE :: LogAction (Widget HTML) Message -> IO IDEState
emptyIDE logger = do
  trace  <- newTVarIO emptyTrace
  displayUnreachable <- newEmptyTMVarIO
  let displayUnreachableVal = True

  steps  <- newEmptyTMVarIO
  let stepsVal = 0

  activeNodeQueue <- newTQueueIO

  activeExample <- newEmptyTMVarIO
  let activeExampleVal = None

  activeInitState <- newEmptyTMVarIO
  let activeInitStateVal = emptyCtx

  let source = pure ()
      runSymExec = \_ _ -> pure emptyTrace

  solving <- newEmptyTMVarIO
  let solve = \_ -> pure emptyTrace

  pure $ IDEState
    trace
    displayUnreachable
    displayUnreachableVal

    steps
    stepsVal

    activeNodeQueue

    activeExample
    activeExampleVal

    activeInitState
    activeInitStateVal

    source
    runSymExec

    solving
    solve

    logger

-- | Here we mimic co-log's loggin functions with implicit params
--   with a hope to refactor the code later to use the actual co-log

-- | Logs the message with given severity @sev@.
log :: Severity -> Text -> App ()
log msgSeverity msgText =
    withFrozenCallStack (logMsg Msg{ msgStack = callStack, .. })
  where
    logMsg msg = do
        let (LogAction log) = _logger ?ide
        log msg
