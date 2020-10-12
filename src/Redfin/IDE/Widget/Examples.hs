{-# LANGUAGE PatternSynonyms #-}

module Redfin.IDE.Widget.Examples
  ( swapExample
  , examplesWidget
  ) where

import           Colog                         (pattern D, pattern E,
                                                HasLog (..), pattern I,
                                                LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                hiding (id)
import qualified Concur.Replica.DOM.Events     as P
import           Control.Applicative           (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative      as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                    as A
import           Data.Either                   (rights)
import           Data.Functor                  (void)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Text
import qualified Data.Text.Read                as Text
import           Prelude                       hiding (div, log, lookup, span)
import           Replica.VDOM.Render           as Render
import           Text.Read                     (readEither)

import           ISA.Types

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Backend.Symbolic.List.Run (runModel)
import qualified ISA.Example.Add               as Example
import qualified ISA.Example.Sum               as ExampleSum

swapExample :: IDEState -> Example -> IDEState
swapExample ide = \case
  ExampleAdd -> ide { _source = Example.addLowLevel
                    , _runSymExec = Example.symexecTrace
                    , _activeExampleVal = ExampleAdd
                    , _stepsVal = 0
                    }
  ExampleSum -> ide { _source = ExampleSum.sumArrayLowLevel
                    , _runSymExec = \s -> runModel s ExampleSum.initContext
                    , _activeExampleVal = ExampleSum
                    , _stepsVal = 0
                    }

examplesWidget :: App a
examplesWidget = do
  log I "Example widget initialised"
  e <- ul [classList [("examplesWidget", True)]]
          [ exampleButton ExampleAdd
          , exampleButton ExampleSum
          -- , li [] [span [] [button [ExampleGCD <$ onClick] [text "GCD"]]]
          -- , li [] [span [] [button [ExampleMotor <$ onClick] [text "Motor"]]]
          ]
  liftIO . atomically $ putTMVar (_activeExample ?ide) e
  examplesWidget
  where exampleButton ex =
          li [] [a [ classList [ ("exampleButton", True)
                               , ("activeExample", ex == _activeExampleVal ?ide)]
                   , ex <$ onClick]
                   [text (Text.pack $ show ex)]]
