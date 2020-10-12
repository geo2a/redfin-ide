{-# LANGUAGE TupleSections #-}
module Redfin.IDE.Widget where

import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica            hiding (id)
import qualified Concur.Replica.DOM.Events as P
import           Control.Applicative       (Alternative, empty, (<|>))
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq, (<|), (|>))
import qualified Data.Sequence             as Seq
import           Data.Set                  (Set)
import qualified Data.Set                  as Set


-- | Run widgets in parallel and wait for all to produce a value
--   OR exit early if the LAST one returns
--   TODO: this is a sub optimal piece of code which might brake and will need refactoring
--   Partially adopted from:
--   https://github.com/purescript-concur/purescript-concur-core/blob/253be02725eac8f3515f75a4542602301d24a749/src/Concur/Core/Types.purs#L156
joinOrLast :: Ord a => [Widget HTML a] -> Widget HTML [a]
joinOrLast = (toList . Set.fromList . toList <$>) . andd' . Seq.fromList
  where
    andd' :: Seq (Widget HTML a) -> Widget HTML (Seq a)
    andd' ws = go ws Set.empty
      where
        go xs is = do
          (i, e) <- Seq.foldrWithIndex (\i w r -> (fmap (i,) w) <|> r) empty ws
          let xs' = Seq.deleteAt i xs
              is' = Set.insert i is
          if
            Set.member (length ws - 1) is'
            then pure (Seq.singleton e)
            else do
              rest <- go xs' is'
              pure $ e <| rest
