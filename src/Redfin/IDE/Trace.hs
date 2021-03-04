{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
module Redfin.IDE.Trace (htmlTrace) where

import           Colog                       (HasLog (..), LogAction, Message,
                                              WithLog, pattern D, pattern I,
                                              richMessageAction)
import           Concur.Core
import           Concur.Replica
import qualified Concur.Replica.DOM.Events   as P
import           Control.Applicative         ((<|>))
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap                 as IntMap
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Prelude                     hiding (div, id, log, span)

import           ISA.Backend.Symbolic.Zipper
import           ISA.Types                   hiding (not)
import           ISA.Types.Context           hiding (Context)
import           ISA.Types.Tree              (Tree (..))

import           ISA.Types.ZeroOneTwo

import           Redfin.IDE.State
import           Redfin.IDE.Types

-- | Build an interactive view of a trace
htmlTrace :: Trace -> App a
htmlTrace trace =
  div [classList [("tree", True)]]
    [ ul [] [htmlTree  (_states trace) (_layout trace)]]

-- | Traverse the layout tree and create interactive nodes
htmlTree :: IntMap Context -> Tree Int () -> App a
htmlTree states = \case
  Leaf n _               -> spawn Zero states n
  Trunk n child          -> spawn (One child) states n
  Branch n lchild rchild -> spawn (Two lchild rchild) states n

spawn :: ZeroOneTwo (Tree Int ()) -> IntMap Context -> Int -> App a
spawn children states n =
  case IntMap.lookup n states  of
    Nothing -> li [classList cs] []
    Just ctx ->
      case children of
        Zero      -> li [classList $ ("leaf", True):cs] [node n]
        One child -> li [classList cs] [node n, ul [] [htmlTree states child]]
        Two lchild rchild ->
          li [classList cs] [node n, ul [] [ htmlTree states lchild
                                           , htmlTree states rchild ]]
  where
     cs = [ ("node", True)
          , ("reachable", maybe False (\x->x) (isReachable <$> IntMap.lookup n states))
          , ("unreachable", maybe False (\x->x) (not . isReachable <$> IntMap.lookup n states))
          , ("hidden", hidden [n] states)
          , ("has-hidden-children", False)
             -- any (\(Tree.Node (Node i ctx) _) ->
             --        (not $ isReachable ctx) && (not $ _displayUnreachableVal ?ide)) ns)
          ]
     hidden xs states =
       any (\ctx -> (not $ isReachable ctx) && (not $ _displayUnreachableVal ?ide))
       . catMaybes . map (\n -> IntMap.lookup n states) $ xs

-- | Display a layout node as a clickable number that triggers the display
--   of the corresponding state
node :: Int -> App a
node n = do
  ev <- a [ Just <$> onClick]
          [ text (Text.pack . show $ n)]
  case ev of
    Nothing  -> node n
    Just e -> do
      liftIO . atomically $
        writeTQueue (_activeNodeQueue ?ide) n
      log D $ "Active node changed to " <> Text.pack (show n)
      node n
