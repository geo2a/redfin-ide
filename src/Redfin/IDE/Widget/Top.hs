{-# LANGUAGE RankNTypes #-}
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
import           Control.Concurrent.STM.TSem
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
import           Replica.VDOM.Types             (DOMEvent (getDOMEvent))
import           Text.Read                      (readEither)

import           Redfin.IDE.Types
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.Top.Examples

data SMTAction = SolvePressed
               | DisplayUnreachableToggled

-- | Top pane of the IDE and its widgets
topPane :: App a
topPane =
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [ examplesWidget
        , symExecWidget
        , smtWidget
        ]

-- | Specify the number of symbolic execution steps
symExecWidget :: App a
symExecWidget = do
  log D "SymExec widget initialised"
  let msg = Text.pack . show $ (_stepsVal ?ide)
  event <- div [classList [ ("widget", True), ("symExecWidget", True)]]
               [ h4 [] [text ("Symbolic simulator")]
               , joinOrLast
                        [ New <$> Text.unpack . targetValue . target <$>
                            input [ placeholder msg, value "", onChange, autofocus True]
                        , Old (_stepsVal ?ide) <$ button [onClick] [text "Run"]
                        ]
               ]
  let news = filter (\case New _ -> True
                           _ -> False) event
  case news of
    [New e] -> case readEither e of
      Left _      -> symExecWidget
      Right steps -> do
        liftIO . atomically $ putTMVar (_steps ?ide) steps
        symExecWidget
    _ -> symExecWidget

-- | Handle SMT solving
smtWidget :: App a
smtWidget = do
  log D "SMT widget initialised"
  e <- div [classList [ ("widget", True), ("SMTWidget", True)
                      ]
           ]
           [ h4 [] [text "SMT Solver"]
           , SolvePressed <$ button [onClick] [text "Solve"]
           , DisplayUnreachableToggled <$
             label [] [ input [type_ "checkbox", checked (_displayUnreachableVal ?ide), onClick]
                      , text "Display unreachable"]
           ]
  case e of
    SolvePressed -> do
      div [classList [ ("widget", True), ("SMTWidget", True)
                     ]
          ] [ liftIO . atomically $ putTMVar (_solving ?ide) ()
            , text "Solving..."]
      smtWidget
    DisplayUnreachableToggled -> do
      let display' = not (_displayUnreachableVal ?ide)
          ide' = ?ide {_displayUnreachableVal = display'}
      liftIO . atomically $ putTMVar (_displayUnreachable ?ide) display'
      let ?ide = ide' in smtWidget
