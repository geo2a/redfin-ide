module Redfin.IDE.Widget.Top (topPane) where

import           Colog                          (pattern D, pattern E,
                                                 HasLog (..), pattern I,
                                                 LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                 hiding (id)
import qualified Concur.Replica.DOM.Events      as P
import           Concur.Replica.DOM.Props
import           Control.Applicative            (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative       as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                     as A
import           Data.Either                    (rights)
import           Data.Functor                   (void)
import qualified Data.Map.Strict                as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy.Builder         as Text
import qualified Data.Text.Read                 as Text
import           Prelude                        hiding (div, log, lookup, span)
import           Replica.VDOM.Render            as Render
import           Text.Read                      (readEither)

import           Redfin.IDE.Types


import           Redfin.IDE.Widget.Top.Examples

data Action = StepsChanged Int
            | SolvePressed
            | DisplayUnreachableToggled

-- | Top pane of the IDE and its widgets
topPane :: App a
topPane = do
  event <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [ examplesWidget
        , symExecWidget
        , smtWidget
        ]
  case event of
    StepsChanged steps' ->
      when (steps' /= (_stepsVal ?ide)) $
        liftIO . atomically $ putTMVar (_steps ?ide) steps'
    SolvePressed ->
        liftIO . atomically $ putTMVar (_solvePressed ?ide) True
    DisplayUnreachableToggled ->
      let display' = not (_displayUnreachableVal ?ide)
          ide' = ?ide {_displayUnreachableVal = display'}
      in do liftIO . atomically $ putTMVar (_displayUnreachable ?ide) display'
            let ?ide = ide' in topPane
  topPane

-- | Specify the number of symbolic execution steps
symExecWidget :: App Action
symExecWidget = do
  let msg = Text.pack . show $ _stepsVal ?ide
  event <- div [classList [ ("widget", True), ("symExecWidget", True)]]
               [ h4 [] [text ("Symbolic execution")]
               , p [] [ label [] [text ("Steps: ")]
                      , Just . targetValue . target <$>
                          input [ placeholder msg, value "", onChange, autofocus True]
                      , Nothing <$ button [onClick] [text "Run"]
                      ]
               ]
  case event of
    Just e -> case (readEither . Text.unpack $ e) of
      Left _      -> symExecWidget
      Right steps -> let ?ide = ?ide {_stepsVal = steps } in symExecWidget
    Nothing -> pure . StepsChanged $ _stepsVal ?ide

-- | Handle SMT solving
smtWidget :: App Action
smtWidget =
  div [classList [ ("widget", True), ("SMTWidget", True)
                 ]
      ]
      [ h4 [] [text "SMT Solving"]
      , SolvePressed <$ button [onClick] [text "Solve"]
      , DisplayUnreachableToggled <$
        label [] [ input [type_ "checkbox", checked (_displayUnreachableVal ?ide), onClick]
                 , text "Display unreachable"]
      ]
