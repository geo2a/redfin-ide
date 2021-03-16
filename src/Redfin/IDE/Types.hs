{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}


module Redfin.IDE.Types where

import           Colog                           (HasLog (..), LogAction (..),
                                                  Message, Severity, WithLog,
                                                  pattern D, pattern I,
                                                  richMessageAction)
import           Colog.Message                   (Msg (..))
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                  hiding (id)
import           Control.Concurrent.STM
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, ReaderT (..))
import           Data.Bifunctor                  (second)
import           Data.Int                        (Int32)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import           GHC.Generics
import           GHC.Stack                       (HasCallStack, callStack,
                                                  withFrozenCallStack)
import qualified Network.Wai.Handler.Replica     as R


import           Data.Aeson                      as JSON

import           ISA.Assembly
import           ISA.Backend.Symbolic.Zipper     hiding (_trace)
import qualified ISA.Backend.Symbolic.Zipper     as Engine
import           ISA.Backend.Symbolic.Zipper.Run
import           ISA.Types
import           ISA.Types.Context               hiding (Context)
import           ISA.Types.Instruction           hiding (Add)
import qualified ISA.Types.Instruction.Encode    as ISA
import           ISA.Types.Key
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Tree
import           ISA.Types.ZeroOneTwo

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
             | MotorLoop
             deriving (Generic, Eq)

instance Show Example where
  show = \case
    None      -> "Custom"
    Add       -> "Add"
    Sum       -> "Sum"
    MotorLoop -> "MotorLoop"


instance ToJSON Example where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Example where

type NodeId = Int

_trace :: IDEState -> TVar Trace
_trace = Engine._trace . _engine

data IDEState =
  IDEState { _engine                :: EngineState
           , _traceChanged          :: TMVar (Maybe (Tree Int ()))

           , _displayUnreachable    :: TMVar Bool
           , _displayUnreachableVal :: Bool

           , _steps                 :: TMVar Steps
           , _stepsVal              :: Steps

           , _timeout               :: TMVar Int
           , _timeoutVal            :: Int

           , _activeNode            :: TVar NodeId

           , _activeExample         :: TMVar Example
           , _activeExampleVal      :: Example

           , _activeInitState       :: TMVar Context
           , _activeInitStateVal    :: Context

           , _source                :: [(CAddress, Instruction (Data Int32))]

           , _solving               :: TMVar ()

           , _savePrefix            :: FilePath

           }

type App a = ( HasCallStack
             , ?logger :: LogAction (Widget HTML) Message
             -- Browser context, for running JS callbacks
             , ?client :: R.Context
             , ?ide :: IDEState) => Widget HTML a

emptyStats :: SymExecStats
emptyStats = MkSymExecStats 0

emptyIDE :: IO IDEState
emptyIDE = do
  trace  <- newTVarIO emptyTrace
  engine <- mkEngineState trace
  traceChanged <- newEmptyTMVarIO
  displayUnreachable <- newEmptyTMVarIO
  let displayUnreachableVal = True

  steps  <- newEmptyTMVarIO
  let stepsVal = 0

  timeout <- newEmptyTMVarIO
  let timeoutVal = 100

  activeNode <- newTVarIO (-1)

  activeExample <- newEmptyTMVarIO
  let activeExampleVal = None

  activeInitState <- newEmptyTMVarIO
  let activeInitStateVal = emptyCtx

  let source = []

  solving <- newEmptyTMVarIO

  pure $ IDEState
    engine

    traceChanged

    displayUnreachable
    displayUnreachableVal

    steps
    stepsVal

    timeout
    timeoutVal

    activeNode

    activeExample
    activeExampleVal

    activeInitState
    activeInitStateVal

    source

    solving

    "/home/geo2a/Desktop/redfin-ide/"



-- | Here we mimic co-log's loggin functions with implicit params
--   with a hope to refactor the code later to use the actual co-log

-- | Logs the message with given severity @sev@.
log :: Severity -> Text -> App ()
log msgSeverity msgText =
    withFrozenCallStack (logMsg Msg{ msgStack = callStack, .. })
  where
    logMsg msg = do
        let (LogAction log) = ?logger
        log msg
