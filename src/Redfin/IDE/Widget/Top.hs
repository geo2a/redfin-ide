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
import           Control.Concurrent
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
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy.Builder         as Text
import qualified Data.Text.Read                 as Text
import           Prelude                        hiding (div, id, log, lookup,
                                                 span)
import           Replica.VDOM.Render            as Render
import           Replica.VDOM.Types             (DOMEvent (getDOMEvent))
import           Text.Read                      (readEither)

import           Redfin.IDE.Types
import           Redfin.IDE.Types.Save
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.Top.Examples

data Action = StepsChanged String
            | TimeoutChanged String
            | RunPressed
            | DisplayUnreachableToggled

-- | Top pane of the IDE and its widgets
topPane :: App IDEState
topPane =
  section [classList [ ("pane", True), ("toppane", True)]]
    [ div [classList [ ("contents", True)]]
        [ saveWidget ("", "") Nothing
        , examplesWidget
        , symExecWidget (_stepsVal ?ide) (_timeoutVal ?ide)
        ]
    ]

-- | Save and restore the IDE state from file
saveWidget :: (FilePath, FilePath) -> Maybe Text -> App IDEState
saveWidget (save, load) msg = do
  let prefix = _savePrefix ?ide
  div [classList [("box", True), ("saveWidget", True)]]
    [ div [] [ h3 [] [text "Project"]
             , Just . Right <$> div [] [ Nothing <$ button [onClick] [text "Load"]
                                , Just . getValue <$> input [value (Text.pack load)
                                                            , placeholder "path/to/project.json"
                                                            , onChange]]
             , Just . Left <$> div [] [ Nothing <$ button [onClick] [text "Save"]
                               , Just . getValue <$> input [value (Text.pack save)
                                                           , placeholder "path/to/project.json"
                                                           , onChange]]
             , Nothing <$ span [classList [("notice", True)]]
                               [ maybe empty text msg
                               , liftIO (threadDelay $ 5 * 10^6)]
             ]
    ] >>= \case Just (Left Nothing) -> do
                  log D $ "IDE saved into" <> Text.pack save
                  liftIO (saveIDE (prefix <> save) ?ide) >>= \case
                    Left err -> saveWidget (save, load) (Just err)
                    Right () -> saveWidget (save, load) (Just "Saved successfully")
                Just (Left (Just save')) ->
                  saveWidget (save', load) Nothing
                Just (Right Nothing) -> do
                  log D $ "Loading IDE from " <> Text.pack load
                  liftIO (loadIDE (prefix <> load)) >>= \case
                    Left err -> saveWidget (save, load) (Just err)
                    Right ide -> pure ide
                Just (Right (Just load')) ->
                  saveWidget (save, load') Nothing
                Nothing -> saveWidget (save, load) Nothing

-- | Specify the number of symbolic execution steps
symExecWidget :: Steps -> Int -> App a
symExecWidget steps timeout = do
  log D "SymExec widget initialised"
  widget >>= \case
    StepsChanged e ->
      case readEither e of
        Left _      -> symExecWidget steps timeout
        Right newSteps ->
          symExecWidget newSteps timeout
    StepsChanged e ->
      case readEither e of
        Left _      -> symExecWidget steps timeout
        Right newTimeout ->
          symExecWidget steps newTimeout
    RunPressed -> do
      div [classList [ ("box", True), ("symExecWidget", True)]]
          [ h4 [] [ text ("Symbolic simulator")]
          , liftIO . atomically $ putTMVar (_steps ?ide) steps
          , text "Executing..."]
      symExecWidget steps timeout
    DisplayUnreachableToggled -> do
      let display' = not (_displayUnreachableVal ?ide)
          ide' = ?ide {_displayUnreachableVal = display'}
      liftIO . atomically $ putTMVar (_displayUnreachable ?ide) display'
      let ?ide = ide' in symExecWidget steps timeout
  where
    stepsTxt = Text.pack . show $ steps
    timeoutTxt = Text.pack . show $ timeout
    widget =
      div [classList [ ("box", True), ("symExecWidget", True)]]
          [ h4 [] [text ("Symbolic simulator")]
          , StepsChanged <$> Text.unpack . targetValue . target <$>
                       p [] [ label [] [text "Steps: "]
                            , input [placeholder stepsTxt, value stepsTxt, onChange]
                            ]
          , TimeoutChanged <$> Text.unpack . targetValue . target <$>
                       p [] [ label [] [text "Timeout (s): "]
                            , input [placeholder timeoutTxt, value timeoutTxt, onChange]
                            ]
          , RunPressed <$ button [onClick] [text "Run"]
          , DisplayUnreachableToggled <$
              label [] [ input [type_ "checkbox", checked (_displayUnreachableVal ?ide), onClick]
                       , text "Display unreachable"]
          ]

-- -- | Handle SMT solving
-- smtWidget :: App a
-- smtWidget = do
--   log D "SMT widget initialised"
--   e <- div [classList [ ("widget", True), ("SMTWidget", True)
--                       ]
--            ]
--            [ h4 [] [text "SMT Solver"]
--            , SolvePressed <$ button [onClick] [text "Solve"]
--            ,
--            ]
--   case e of
--     SolvePressed -> do
--       div [classList [ ("widget", True), ("SMTWidget", True)
--                      ]
--           ] [ liftIO . atomically $ putTMVar (_solving ?ide) ()
--             , text "Solving..."]
--       smtWidget
--     DisplayUnreachableToggled -> do
--       let display' = not (_displayUnreachableVal ?ide)
--           ide' = ?ide {_displayUnreachableVal = display'}
--       liftIO . atomically $ putTMVar (_displayUnreachable ?ide) display'
--       let ?ide = ide' in smtWidget
