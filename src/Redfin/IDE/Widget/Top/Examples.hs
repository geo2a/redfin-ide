module Redfin.IDE.Widget.Top.Examples
  ( swapExample
  , examplesWidget
  ) where

import           Colog                           (HasLog (..), LogAction,
                                                  Message, pattern D, pattern E,
                                                  pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                  hiding (defaultValue, id)
import qualified Concur.Replica.DOM.Events       as P
import           Control.Applicative             (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative        as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                      as A
import           Data.Either                     (rights)
import           Data.Functor                    (void)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
import qualified Data.Text.Read                  as Text
import           Prelude                         hiding (div, log, lookup, span)
import           Replica.VDOM.Render             as Render
import           Text.Read                       (readEither)

import           ISA.Types

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly                    hiding (div)
import           ISA.Backend.Symbolic.Zipper
import           ISA.Backend.Symbolic.Zipper.Run (runModel)
import qualified ISA.Example.Add                 as EAdd
import qualified ISA.Example.MotorControl        as ELoop
import qualified ISA.Example.Sum                 as ESum

swapExample :: Example -> App IDEState
swapExample = \case
  None -> liftIO emptyIDE
  Add -> do
    liftIO . atomically . writeTVar (_executor ?ide) $
      defaultValue {_executorInitState = EAdd.initCtx}
    pure ?ide { _source = assemble $ EAdd.addLowLevel
             , _activeExampleVal = Add
             }
  Sum -> do
    liftIO . atomically . writeTVar (_executor ?ide) $
      defaultValue {_executorInitState = ESum.initCtx}
    pure ?ide { _source = assemble $ ESum.sumArrayLowLevel
             , _activeExampleVal = Sum
             }
  MotorLoop -> do
    liftIO . atomically . writeTVar (_executor ?ide) $
      defaultValue {_executorInitState = ELoop.initCtx}
    pure ?ide { _source = assemble $ ELoop.mc_loop
             , _activeExampleVal = MotorLoop
             }

examplesWidget :: App a
examplesWidget = do
  log I "Example widget initialised"
  e <- div [classList [("box", True), ("examplesWidget", True)]]
           [ h4 [] [text "Examples"]
           , div [classList [("examples", True)]]
                 [ tooltipped "Add two numbers" $ exampleButton Add
                 , tooltipped "Sum of an array of known length" $ exampleButton Sum
                 , tooltipped "Loop invariant for stepper-motor control program" $
                     exampleButton MotorLoop
                 , tooltipped "Sandbox" $
                     exampleButton None
                 ]
           ]
  liftIO . atomically $ writeTQueue (_events ?ide) (ExampleChanged e)
  examplesWidget
  where exampleButton ex =
          button [ classList [ ("exampleButton", True)
                             , ("activeExample", ex == _activeExampleVal ?ide)]
                 , ex <$ onClick]
          [text (Text.pack $ show ex)]
