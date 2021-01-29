{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}


module Redfin.IDE.Types where

import           Colog                       (pattern D, HasLog (..), pattern I,
                                              LogAction (..), Message, Severity,
                                              WithLog, richMessageAction)
import           Colog.Message               (Msg (..))
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT (..))
import           Data.Aeson
import           GHC.Generics

import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica              hiding (id)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Data.Int                    (Int32)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import           GHC.Stack                   (HasCallStack, callStack,
                                              withFrozenCallStack)

import           ISA.Assembly
import           ISA.Types
import           ISA.Types.Instruction
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
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
             deriving (Generic, Show, Eq)

instance ToJSON Example where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Example where
-- -- | Pure values of the current IDE state
-- --   This datatypes' accessors will never be used in the code directly,
-- --   instead the fields will be accessed through smart accessors of 'IDEState'
-- data IDEVals =
--   MkIDEVals { dispUnrVal :: Bool
--             , stps    :: Steps
--             , tmout   :: Int
--             , actEx   :: Example
--             , actSt   :: Context
--             , src     :: [(Address, Instruction (Data Int32))]
--             } deriving Generic

data IDEState =
  IDEState { _trace                 :: TVar (Trace Context)
           , _displayUnreachable    :: TMVar Bool
           , _displayUnreachableVal :: Bool

           , _steps                 :: TMVar Steps
           , _stepsVal              :: Steps

           , _timeout               :: TMVar Int
           , _timeoutVal            :: Int

           , _activeNodeQueue       :: TQueue NodeId

           , _activeExample         :: TMVar Example
           , _activeExampleVal      :: Example

           , _activeInitState       :: TMVar Context
           , _activeInitStateVal    :: Context

           , _source                :: Script

           , _solving               :: TMVar ()

           }

data Save =
  MkSave { _saveTrace              :: Trace Context
         , _saveDisplayUnreachabel :: Bool
         , _saveSteps              :: Steps
         , _saveTimeout            :: Int
         , _saveActiveNode         :: NodeId
         , _saveExample            :: Example
         , _saveInitState          :: Context
         , _saveSource             :: [(Address, InstructionCode)]
         } deriving Generic

instance ToJSON Save


type App a = ( HasCallStack
             , ?logger :: LogAction (Widget HTML) Message
             , ?ide :: IDEState) => Widget HTML a

emptyCtx :: Context
emptyCtx = MkContext Map.empty (SConst (CBool True)) [] Nothing

emptyTrace :: Trace Context
emptyTrace = mkTrace (Node 0 emptyCtx) []

emptyStats :: SymExecStats
emptyStats = MkSymExecStats 0

emptyIDE :: IO IDEState
emptyIDE = do
  trace  <- newTVarIO emptyTrace
  displayUnreachable <- newEmptyTMVarIO
  let displayUnreachableVal = True

  steps  <- newEmptyTMVarIO
  let stepsVal = 0

  timeout <- newEmptyTMVarIO
  let timeoutVal = 100

  activeNodeQueue <- newTQueueIO

  activeExample <- newEmptyTMVarIO
  let activeExampleVal = None

  activeInitState <- newEmptyTMVarIO
  let activeInitStateVal = emptyCtx

  let source = pure ()

  solving <- newEmptyTMVarIO

  pure $ IDEState
    trace
    displayUnreachable
    displayUnreachableVal

    steps
    stepsVal

    timeout
    timeoutVal

    activeNodeQueue

    activeExample
    activeExampleVal

    activeInitState
    activeInitStateVal

    source

    solving

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
