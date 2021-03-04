{-# LANGUAGE RankNTypes #-}
module Redfin.IDE.Widget.Top.Verification where

import           Colog                          (HasLog (..), LogAction,
                                                 Message, pattern D, pattern E,
                                                 pattern I)
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
import           Data.Bifunctor
import           Data.Either                    (rights)
import           Data.Functor                   (void)
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy.Builder         as Text
import qualified Data.Text.Read                 as Text
import           Prelude                        hiding (div, id, log, lookup,
                                                 span)
import           Replica.VDOM.Render            as Render
import           Replica.VDOM.Types             (DOMEvent (getDOMEvent))
import           Text.Read                      (readEither)

import           Redfin.IDE.State               (fancyConstraints,
                                                 fancySolution)
import           Redfin.IDE.Types
import           Redfin.IDE.Types.Save
import           Redfin.IDE.Widget

import           ISA.Backend.Symbolic.Zipper
import           ISA.Types.Context              hiding (Context)
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Logic.Adhoc
import           ISA.Types.Symbolic.Parser

-- data Action = PropertyChanged Text
--             | ProvePressed

-- box :: [Widget HTML a] -> Widget HTML a
-- box xs =
--   div [classList [("box", True), ("verificationWidget", True)]]
--       ((h4 [] [text "Verifier"]) : xs)

-- control :: (Maybe Text, Text, Maybe Proof) -> Widget HTML Action
-- control (err, propTxt, prfTxt) = div []
--   [ PropertyChanged . targetValue . target <$>
--       p [] [ label [] [text "Property: "]
--            , input [onChange, placeholder propTxt, value propTxt]
--            ]
--   , ProvePressed <$ button [onClick] [text "Prove"]
--   ]

-- errorNotice :: Maybe Text -> Widget HTML ()
-- errorNotice err = span [classList [("notice", True)]]
--               [ maybe empty text err
--               , liftIO (threadDelay $ 5 * 10^6)]

-- getTheorem :: Proof -> Theorem
-- getTheorem = \case Proved thm        -> thm
--                    Falsifiable thm _ -> thm

-- getStmt :: Theorem -> [(Text, Sym)]
-- getStmt (MkTheorem _ exprs) =
--   map (first (\n -> "In node " <> Text.pack (show n) <> ": ")) $
--   map (\(Node n b) -> (n, b)) $ Set.toList exprs

-- displayProof = \case
--   Proved _ -> [h5 [] [text "Q.E.D."]]
--   Falsifiable _ (nid, r) ->
--     case r of
--       Unsatisfiable -> empty
--       Satisfiable model ->
--         [ h5 [] [text "Falsifiable!"]
--         , ul [] (map (\(name,x) ->
--                  li [] [text $ name <> " = " <>
--                         Text.pack (show x)]) (Map.assocs $ modelAssocs model))
--         ]

-- proof :: Maybe Proof -> Widget HTML a
-- proof = \case
--   Nothing -> empty
--   Just prf ->
--     div [classList [("proof", True)]]
--         [ div []
--             [ h5 [] [text "Proof for: "]
--             , fancyConstraints . getStmt . getTheorem $ prf]
--         , div [] (displayProof prf)
--         ]

-- verificationWidget :: (Maybe Text, Text, Maybe Proof) -> Maybe Property -> App a
-- verificationWidget args@(err, propTxt, prf) prop = do
--   e <- box [ Just <$> control args
--            , Nothing <$ errorNotice err
--            , proof prf
--            ]
--   case e of
--     Nothing -> verificationWidget (err, propTxt, prf) prop
--     Just (PropertyChanged newPropTxt) -> do
--       log D $ "New property: " <> propTxt
--       case (parseProp "" newPropTxt) of
--         Left  err     -> verificationWidget (Just err, newPropTxt, Nothing) prop
--         Right newProp -> verificationWidget (Nothing, newPropTxt, Nothing) (Just newProp)
--     Just ProvePressed -> do
--       tr <- liftIO $ readTVarIO (_trace ?ide)
--       let pr = maybe undefined (\x -> x) prop
--       case (formulate pr tr) of
--         Left err      -> verificationWidget (Just err, propTxt, Nothing) prop
--         Right theorem -> do
--           prf <- box [ p [] [ text "Proving..."
--                             , liftIO $ prove theorem
--                               (ConstrainBy . map snd . _constraints $ _activeInitStateVal ?ide)
--                             ]]
--           verificationWidget (Nothing, propTxt, Just prf) prop
