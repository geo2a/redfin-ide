module Redfin.IDE.State where

import           Concur.Core
import           Concur.Replica
import qualified Concur.Replica.DOM.Events    as P
import           Control.Applicative          ((<|>))
import           Control.Monad.State
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Tree                    as Tree
import           Prelude                      hiding (div, id, span)

import           ISA.Types
import           ISA.Types.Instruction.Decode (toInstruction)
import           ISA.Types.Symbolic           (Sym (..))
import           ISA.Types.Symbolic.Context   hiding (showIR)
import           ISA.Types.Symbolic.Trace

showIR :: Sym -> Text
showIR v =
  case toInstruction v of
    Left _  -> "uninitialised"
    Right i -> Text.pack $ show i

fancyPathConstraint :: Sym -> Widget HTML a
fancyPathConstraint =
  ul [classList [("pathConstraint" , True)]] .
  map (li [] . (:[])) .
  reverse . map conjunct . splitToplevelConjs

-- | Any path constrain will ALWAYS be a left-associated conjunction
-- of terms. For fancy displaying purposes we want to split it into
-- these terms
splitToplevelConjs :: Sym -> [Sym]
splitToplevelConjs = go []
  where go xs = \case
          SAnd x y -> go (x:xs) y
          SAny x   -> (SAny x):xs
          SConst x -> (SConst x):xs
          _ -> undefined

conjunct :: Sym -> Widget HTML a
conjunct = \case
  (SAdd x y) -> binop " + " x y
  (SSub x y) -> binop " - " x y
  (SMul x y) -> binop " * " x y
  (SDiv x y) -> binop " / " x y
  (SMod x y) -> unop " % " x
  (SAbs x  ) -> text "|" <|> span [] [conjunct x] <|> text "|"

  (SConst x) -> text . Text.pack . show $ x
  (SAny n  ) -> text n

  (SAnd x y) -> binop " ∧ " x y
  (SOr  x y) -> binop " ∨ " x y
  (SEq  x y) -> binop " == " x y
  (SGt  x y) -> binop " > " x y
  (SLt  x y) -> binop " < " x y
  (SNot b  ) -> unop "¬ " b

  where
    unop op x = span [classList [("conjunct", True)]]
                [ span [] [text "("]
                , span [] [text op]
                , span [] [conjunct x]
                , span [] [text ")"]
                ]
    binop op x y = span [classList [("conjunct", True)]]
                   [ span [] [text "("]
                   , span [] [conjunct x]
                   , span [] [text op]
                   , span [] [conjunct y]
                   , span [] [text ")"]
                   ]

displayContext :: Maybe Context -> Widget HTML a
displayContext x =
  case x of
    Nothing -> text $ "Oops: no such state in the trace"
    Just (MkContext vars pc) ->
      let ir = Map.findWithDefault 0 IR vars
          h  = Text.pack . show $ Map.findWithDefault 0 (F Halted) vars
          c  = Text.pack . show $ Map.findWithDefault 0 (F Condition) vars
          o  = Text.pack . show $ Map.findWithDefault 0 (F Overflow) vars
      in ul [classList [ ("context", True)]]
            [ li [] [keyTag IR, span [] [text $ " : " <> showIR ir]]
            , li [] [
                h4 [] [text "Flags"],
                ul []
                  [ li [] [keyTag (F Halted), span [] [text $ " : " <> h]]
                  , li [] [keyTag (F Condition), span [] [text $ " : " <> c]
                  , li [] [keyTag (F Overflow), span [] [text $ " : " <> o]]]
                  ]
                ]
            , li [] [
                h4 [] [text "Path Constraint"],
                fancyPathConstraint pc
                ]
            ]
  where
    keyTag key = span [classList [("keyTag", True)]]
                      [text $ Text.pack $ show key]
