module Redfin.IDE.Types where

import           Colog.Core                 (LogAction (..))
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica             hiding (id)
import           Control.Concurrent.STM
import           Data.Text                  (Text)

import           ISA.Assembly
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace

-- | Either-like datatype for tracking new/old content of tags
data Contents a b = Old a
                  | New b
                  deriving (Show, Eq)

type Steps = Int

data Example = ExampleAdd
             | ExampleSum
             -- | ExampleGCD
             -- | ExampleMotor
             deriving (Show, Eq)

data IDEState =
  IDEState { _trace         :: TVar (Trace Context)
           , _steps         :: TMVar Steps
           , _activeNode    :: TMVar NodeId
           , _activeExample :: TMVar Example

           , _source        :: Script
           , _runSymExec    :: Steps -> Trace Context
           }

type App a = LogAction IO Text -> IDEState -> Widget HTML a
