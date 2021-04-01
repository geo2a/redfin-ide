{-# LANGUAGE RankNTypes #-}
module Redfin.IDE.Widget.Top.Verification where

import           Colog                         (HasLog (..), LogAction, Message,
                                                pattern D, pattern E, pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                hiding (id)
import qualified Concur.Replica.DOM.Events     as P
import           Concur.Replica.DOM.Props
import           Control.Applicative           (Alternative, empty, (<|>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative      as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                    as A
import           Data.Bifunctor
import           Data.Either                   (rights)
import           Data.Functor                  (void)
import           Data.IntSet                   (IntSet)
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Text
import qualified Data.Text.Read                as Text
import           Prelude                       hiding (div, id, log, lookup,
                                                span)
import           Replica.VDOM.Render           as Render
import           Replica.VDOM.Types            (DOMEvent (getDOMEvent))
import           Text.Read                     (readEither)

import           Redfin.IDE.State              (fancyConstraints, fancySolution)
import           Redfin.IDE.Types
import           Redfin.IDE.Types.Save
import           Redfin.IDE.Widget

import           ISA.Backend.Symbolic.Zipper   hiding (_trace)
import           ISA.Types.Context             hiding (Context)
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.ACTL
import           ISA.Types.Symbolic.ACTL.Model
import           ISA.Types.Symbolic.Parser

data Action = PropertyChanged Text
            | ProvePressed

box :: [Widget HTML a] -> Widget HTML a
box xs =
  div [classList [("box", True), ("verificationWidget", True)]]
      ((h4 [] [text "Verifier"]) : xs)

control :: VerifierState -> Widget HTML Action
control (VerifierState err propTxt _ prfTxt _ _) = div []
  [ PropertyChanged . targetValue . target <$>
      div [] [ label [] [text "Property: "]
             , input [onChange, placeholder propTxt, value propTxt]
             ]
  , ProvePressed <$ button [onClick] [text "Prove"]
  ]

errorNotice :: Maybe Text -> Widget HTML ()
errorNotice err =
  span [classList [("notice", True)]]
       [maybe empty text err]

getTheorem :: Proof -> ACTL
getTheorem = \case Proved thm        -> thm
                   Falsifiable thm _ -> thm

displayProof :: Proof -> Widget HTML a
displayProof = \case
  Proved _ -> h5 [] [text "Q.E.D."]
  Falsifiable _ contra ->
    div [] $
      [ h5 [] [text "Falsifiable!"]
      , text (Text.pack $ "Violation at states: " <> show (map fst contra))
      ]

proof :: Maybe Proof -> Widget HTML a
proof = \case
  Nothing -> empty
  Just prf ->
    div [classList [("proof", True)]]
        [ displayProof prf
        ]

verificationWidget :: App a
verificationWidget = do
  args@(VerifierState err propTxt thm prf contra history) <- liftIO $ readTVarIO (_verifier ?ide)
  trace <- liftIO $ readTVarIO (_trace ?ide)
  e <- box [ Just <$> control args
           , Nothing <$ errorNotice err
           , proof prf
           ]
  case e of
    Nothing -> verificationWidget
    Just (PropertyChanged newPropTxt) -> do
      log D $ "New property: " <> newPropTxt
      case (parseTheorem "" newPropTxt) of
        Left  err     -> do
          liftIO . atomically $
            writeTVar (_verifier ?ide) (args { _verifierError = Just err
                                             , _verifierPropTxt = newPropTxt
                                             , _verifierProp = thm
                                             , _verifierProof = Nothing })
          verificationWidget
        Right newThm -> do
          liftIO . atomically $
            writeTVar (_verifier ?ide) (args { _verifierError = Nothing
                                             , _verifierPropTxt = newPropTxt
                                             , _verifierProp = Just newThm
                                             , _verifierProof = Nothing })
          verificationWidget
    Just ProvePressed -> do
      tr <- liftIO $ readTVarIO (_trace ?ide)
      case thm of
        Nothing -> do
          log D "undefined theorem"
          liftIO . atomically $
            writeTVar (_verifier ?ide) (args { _verifierError = Nothing
                                             , _verifierPropTxt = propTxt
                                             , _verifierProp = Nothing
                                             , _verifierProof = Nothing })
          verificationWidget
        Just t -> do
          prf <- box [ p [] [ text "Proving..."
                            , liftIO $ prove trace t
                            ]]
          liftIO . atomically $ do
            writeTVar (_verifier ?ide) (args { _verifierError = Nothing
                                             , _verifierPropTxt = propTxt
                                             , _verifierProp = Just t
                                             , _verifierProof = Just prf
                                             , _verifierContra = getContra prf})
            processProof (_events ?ide) prf
          verificationWidget

getContra :: Proof -> IntSet
getContra = \case
  Proved _             -> mempty
  Falsifiable _ contra -> IntSet.fromList $ map fst contra

processProof :: TQueue IDEEvent -> Proof -> STM ()
processProof events = \case
  Proved prop -> do
    -- log I $ "Property " <> Text.pack (show prop) <> " proved"
      writeTQueue events (ContraChanged mempty)
  Falsifiable prop contra -> do
    -- log I $ "Property " <> Text.pack (show prop) <> " falsifiable! " <>
    --         "Counterexample updated with: " <> Text.pack (show $ map fst contra)
      writeTQueue events (ContraChanged . IntSet.fromList $ map fst contra)
