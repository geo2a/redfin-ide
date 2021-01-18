module Redfin.IDE.Widget.Top (topPane) where

import           Colog                          (pattern D, pattern E,
                                                 HasLog (..), pattern I,
                                                 LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                 hiding (id)
import qualified Concur.Replica.DOM.Events      as P
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

data ActionTag = StepsChanged
               | SolvePressed
               | RunPressed

topPane :: App a
topPane = do
  event <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [ examplesWidget
        , (StepsChanged,) <$> symExecWidget (Right (_stepsVal ?ide))
        , (SolvePressed,0) <$ smtWidget
        ]
  case event of
    (StepsChanged, steps') ->
      when (steps' /= (_stepsVal ?ide)) $
        liftIO . atomically $ putTMVar (_steps ?ide) steps'
    (SolvePressed, _) ->
        -- log I $ "Solve pressed"
        liftIO . atomically $ putTMVar (_solvePressed ?ide) True
    _ -> do
      log E "unknown action"
      pure ()
  topPane

-- | Specify the number of symbolic execution steps
symExecWidget :: Either Text Steps -> Widget HTML Steps
symExecWidget s = do
  let msg = either (const "") (Text.pack . show) s
  event <-
         div [classList [ ("widget", True), ("symExecWidget", True)]]
             [ h4 [] [text ("Symbolic execution")]
             , p [] [ label [] [text ("Steps: ")]
                    , (StepsChanged,) . targetValue . target <$> input [ placeholder msg
                                          , value ""
                                          , onChange, autofocus True]
                    , (RunPressed,"") <$ button [onClick] [text "Run"]
                    ]
             -- , either (const $ text "Error: invalid input") (const empty) s
             ]
  case event of
    (StepsChanged, e) -> case (readEither . Text.unpack $ e) of
      Left err -> symExecWidget (Left (Text.pack err))
      Right x  -> symExecWidget (Right x)
    (RunPressed,_) -> case s of
      Left err -> symExecWidget (Left err)
      Right x  -> pure x


smtWidget :: Widget HTML Bool
smtWidget =
  div [classList [ ("widget", True), ("SMTWidget", True)
                 ]
      ]
      [ h4 [] [text "SMT Solving"]
      , p [] [True <$ button [onClick] [text "Solve"]]
      ]
