{-# LANGUAGE RankNTypes #-}
module Redfin.IDE.Widget.Top.Verification where

import           Colog                       (HasLog (..), LogAction, Message,
                                              pattern D, pattern E, pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica              hiding (id)
import qualified Concur.Replica.DOM.Events   as P
import           Concur.Replica.DOM.Props
import           Control.Applicative         (Alternative, empty, (<|>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative    as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                  as A
import           Data.Bifunctor
import           Data.Either                 (rights)
import           Data.Functor                (void)
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy.Builder      as Text
import qualified Data.Text.Read              as Text
import           Prelude                     hiding (div, id, log, lookup, span)
import           Replica.VDOM.Render         as Render
import           Replica.VDOM.Types          (DOMEvent (getDOMEvent))
import           Text.Read                   (readEither)

import           Redfin.IDE.State            (fancyConstraints, fancySolution)
import           Redfin.IDE.Types
import           Redfin.IDE.Types.Save
import           Redfin.IDE.Widget

import           ISA.Backend.Symbolic.Zipper hiding (_trace)
import           ISA.Types.Context           hiding (Context)
import           ISA.Types.SBV
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Parser
import           ISA.Types.Symbolic.Property
import           ISA.Types.Symbolic.SMT

data Action = PropertyChanged Text
            | ProvePressed

box :: [Widget HTML a] -> Widget HTML a
box xs =
  div [classList [("box", True), ("verificationWidget", True)]]
      ((h4 [] [text "Verifier"]) : xs)

control :: (Maybe Text, Text, Maybe Proof) -> Widget HTML Action
control (err, propTxt, prfTxt) = div []
  [ PropertyChanged . targetValue . target <$>
      div [] [ label [] [text "Property: "]
             , input [onChange, placeholder propTxt, value propTxt]
             ]
  , ProvePressed <$ button [onClick] [text "Prove"]
  ]

errorNotice :: Maybe Text -> Widget HTML ()
errorNotice err = span [classList [("notice", True)]]
              [ maybe empty text err
--              , liftIO (threadDelay $ 5 * 10^6)
              ]

getTheorem :: Proof -> Theorem
getTheorem = \case Proved thm        -> thm
                   Falsifiable thm _ -> thm

displayProof = \case
  Proved _ -> [h5 [] [text "Q.E.D."]]
  Falsifiable _ contra ->
    -- case r of
    --   Unsatisfiable -> empty
    --   Satisfiable model ->
    [ h5 [] [text "Falsifiable!"]
    , text (Text.pack $ show contra)
    ]

proof :: Maybe Proof -> Widget HTML a
proof = \case
  Nothing -> empty
  Just prf ->
    div [classList [("proof", True)]]
        [ div [] (displayProof prf)
        ]

verificationWidget :: (Maybe Text, Text, Maybe Proof) -> Maybe Theorem -> App a
verificationWidget args@(err, propTxt, prf) thm = do
  trace <- liftIO $ readTVarIO (_trace ?ide)
  e <- box [ Just <$> control args
           , Nothing <$ errorNotice err
           , proof prf
           ]
  case e of
    Nothing -> verificationWidget (err, propTxt, prf) thm
    Just (PropertyChanged newPropTxt) -> do
      log D $ "New property: " <> propTxt
      case (parseTheorem "" newPropTxt) of
        Left  err     -> verificationWidget (Just err, newPropTxt, Nothing) thm
        Right newProp -> verificationWidget (Nothing, newPropTxt, Nothing) (Just newProp)
    Just ProvePressed -> do
      tr <- liftIO $ readTVarIO (_trace ?ide)
      case thm of
        Nothing -> do
          log D "undefined theorem"
          verificationWidget (Nothing, propTxt, Nothing) thm
        Just t -> do
          prf <- box [ p [] [ text "Proving..."
                            , liftIO $ prove t trace
                            ]]
          log D $ "New proof: " <> (Text.pack $ show prf)
          verificationWidget (Nothing, propTxt, Just prf) thm
