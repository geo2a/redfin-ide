{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
module Redfin.IDE.Trace where

import           Colog                      (pattern D, HasLog (..), pattern I,
                                             LogAction, Message, WithLog,
                                             richMessageAction)
import           Concur.Core
import           Concur.Replica
import qualified Concur.Replica.DOM.Events  as P
import           Control.Applicative        ((<|>))
import           Control.Concurrent.STM
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Tree                  as Tree
import           Prelude                    hiding (div, id, log, span)

import           ISA.Types                  hiding (not)
import           ISA.Types.Symbolic.Context hiding (showIR)
import           ISA.Types.Symbolic.Trace

import           Redfin.IDE.State
import           Redfin.IDE.Types

enumTree :: Tree.Tree a -> Tree.Tree (a,Int)
enumTree = flip evalState 0 . traverse count
  where
    count :: a -> State Int (a,Int)
    count a = do
      i <- get; put (i+1)
      return (a,i)

node :: (Context, Int) -> App a
node args@(ctx, n) = do
  ev <- a [ Just <$> onClick]
          [ text (Text.pack . show $ n)]
  case ev of
    Nothing  -> node args
    Just e -> do
      liftIO . atomically $
        writeTQueue (_activeNodeQueue ?ide) n
      log D $ "Active node changed to " <> Text.pack (show n)
      node args

htmlTree :: Tree.Tree (Context, Int) -> App a
htmlTree (Tree.Node n@(ctx,i) ns) =
  case ns of
    []       -> li [classList $ ("leaf", True):cs] [node n]
    children -> li [classList cs] [node n, ul [] (map htmlTree children)]
  where cs = [ ("node", True)
             , ("reachable", isReachable ctx)
             , ("unreachable", not $ isReachable ctx)
             , ("hidden", (not $ isReachable ctx) && (not $ _displayUnreachableVal ?ide))
             , ("has-hidden-children",
                any (\(Tree.Node (ctx, _) _) -> (not $ isReachable ctx) && (not $ _displayUnreachableVal ?ide)) ns)
             ]

htmlTrace :: Trace Context -> App a
htmlTrace (Trace tree) =
  div [classList [("tree", True)]]
  [ ul [] [htmlTree . enumTree . fmap _nodeBody $ tree]]
