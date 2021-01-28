module Redfin.IDE.Widget.Top.Examples
  ( swapExample
  , examplesWidget
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

import           ISA.Types

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Backend.Symbolic.List.QueryRun (runModel)
import qualified ISA.Example.Add                    as EAdd
import qualified ISA.Example.MotorControl           as ELoop
import qualified ISA.Example.Sum                    as ESum
import           ISA.Types.Symbolic.Trace

swapExample :: IDEState -> Example -> IDEState
swapExample ide = \case
  None -> ide -- TODO: put empty IDE here
  Add -> ide { _source = EAdd.addLowLevel
             , _runSymExec = runModel
             , _activeExampleVal = Add
             , _stepsVal = 0
             , _activeInitStateVal = EAdd.initCtx
             }
  Sum -> ide { _source = ESum.sumArrayLowLevel
             , _runSymExec = runModel
             , _activeExampleVal = Sum
             , _stepsVal = 0
             , _activeInitStateVal = ESum.initCtx
             }
  MotorLoop ->
    ide { _source = ELoop.mc_loop
        , _runSymExec = runModel
        , _activeExampleVal = MotorLoop
        , _stepsVal = 0
        , _activeInitStateVal = ELoop.initCtx
        }

examplesWidget :: App a
examplesWidget = do
  log I "Example widget initialised"
  e <- div [classList [("widget", True), ("examplesWidget", True)]]
           [ h4 [] [text "Examples"]
           , div [classList [("examples", True)]]
                 [ exampleButton Add
                 , exampleButton Sum
                 , exampleButton MotorLoop
                 ]
           ]
  liftIO . atomically $ putTMVar (_activeExample ?ide) e
  examplesWidget
  where exampleButton ex =
          a [ classList [ ("exampleButton", True)
                        , ("activeExample", ex == _activeExampleVal ?ide)]
            , ex <$ onClick]
            [text (Text.pack $ show ex)]
