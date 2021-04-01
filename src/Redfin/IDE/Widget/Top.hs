{-# LANGUAGE RankNTypes #-}
module Redfin.IDE.Widget.Top (topPane) where

import           Colog                              (HasLog (..), LogAction,
                                                     Message, pattern D,
                                                     pattern E, pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                     hiding (id)
import qualified Concur.Replica.DOM.Events          as P
import           Concur.Replica.DOM.Props
import           Control.Applicative                (Alternative, empty, (<|>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
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
import           Prelude                            hiding (div, id, log,
                                                     lookup, span)
import           Replica.VDOM.Render                as Render
import           Replica.VDOM.Types                 (DOMEvent (getDOMEvent))
import           Text.Read                          (readEither)

import           Redfin.IDE.Types                   hiding (IDEEvent (..))
import qualified Redfin.IDE.Types                   as Types
import           Redfin.IDE.Types.Save
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.Top.Examples
import           Redfin.IDE.Widget.Top.Verification

import           ISA.Types
import           ISA.Types.Symbolic                 hiding (getValue)

data Action = StepsChanged String
            | RunPressed

-- | Top pane of the IDE and its widgets
topPane :: App a
topPane =
  section [classList [ ("pane", True), ("toppane", True)]]
    [ div [classList [ ("contents", True)]]
        [ saveWidget ("", "") Nothing
        , examplesWidget
        , symExecWidget
        , verificationWidget
        ]
    ]

-- | Save and restore the IDE state from file
saveWidget :: (FilePath, FilePath) -> Maybe Text -> App a
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
                               , liftIO (threadDelay $ (5 * 10^6))]
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
                    Left err  -> saveWidget (save, load) (Just err)
                    Right ide -> do
                      liftIO . atomically $
                        writeTQueue (_events ?ide) (Types.SaveLoaded ide)
                      saveWidget (save, load) Nothing
                Just (Right (Just load')) ->
                  saveWidget (save, load') Nothing
                Nothing -> saveWidget (save, load) Nothing

-- | Specify the number of symbolic execution steps
symExecWidget :: App a
symExecWidget = do
  steps <- _executorSteps <$> liftIO (readTVarIO (_executor ?ide))
  log D "SymExec widget initialised"
  widget (Text.pack $ show steps) >>= \case
    StepsChanged e ->
      case readEither e of
        Left _      -> symExecWidget
        Right newSteps -> do
          liftIO . atomically $ writeTQueue (_events ?ide) (Types.StepsChanged newSteps)
          symExecWidget
    RunPressed -> do
      div [classList [ ("box", True), ("symExecWidget", True)]]
          [ h4 [] [ text ("Symbolic simulator")]
          -- , liftIO . atomically $ putTMVar (_steps ?ide) steps
          , liftIO . atomically $ writeTQueue (_events ?ide) Types.RunPressed
          , text "Executing..."]
      symExecWidget
  where
    widget stepsTxt =
      div [classList [ ("box", True), ("symExecWidget", True)]]
          [ h4 [] [text ("Symbolic simulator")]
          , StepsChanged <$> Text.unpack . targetValue . target <$>
                       p [] [ label [] [text "Steps: "]
                            , input [placeholder stepsTxt, value stepsTxt, onChange]
                            ]
          , RunPressed <$ button [onClick] [text "Run"]
          ]
