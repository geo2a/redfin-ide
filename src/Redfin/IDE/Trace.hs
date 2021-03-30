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

import           ISA.Backend.Symbolic.Zipper hiding (_trace)
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Key
import           ISA.Types.Prop
import           ISA.Types.Symbolic
import           ISA.Types.Tree              (Tree (..), draw, locKey)

import           ISA.Types.ZeroOneTwo

import           Redfin.IDE.State
import           Redfin.IDE.Types

-- | Build an interactive view of a trace
htmlTrace :: Trace -> App ()
htmlTrace trace = do
  div [classList [("tree", True)]]
    [ ul [] [htmlTree  (_states trace) (_layout trace)]]

-- | Traverse the layout tree and create interactive nodes
htmlTree :: IntMap (Context Sym) -> Tree Int () -> App ()
htmlTree states = \case
  Leaf n _               -> spawn Zero states n
  Trunk n child          -> spawn (One child) states n
  Branch n lchild rchild -> spawn (Two lchild rchild) states n

spawn :: ZeroOneTwo (Tree Int ()) -> IntMap (Context Sym) -> Int -> App ()
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
          , ("unreachable", maybe False (\x->x) (Prelude.not . isReachable <$> IntMap.lookup n states))
          , ("hidden", hidden [n] states)
          , ("has-hidden-children", False)
          , ("halted", maybe False toBool (getBinding (F Halted) =<< (IntMap.lookup n states)))
             -- any (\(Tree.Node (Node i ctx) _) ->
             --        (not $ isReachable ctx) && (not $ _displayUnreachableVal ?ide)) ns)
          ]
     hidden xs states =
       any (\ctx -> (Prelude.not $ isReachable ctx) && (Prelude.not $ _displayUnreachableVal ?ide))
       . catMaybes . map (\n -> IntMap.lookup n states) $ xs

-- | Display a layout node as a clickable number that triggers the display
--   of the corresponding state
node :: Int -> App ()
node n = do
  ev <- orr [ Left . Just <$> a [onClick] [ text (Text.pack . show $ n) ]
            , Right <$> liftIO (atomically fetch)
            ]
  case ev of
    Left Nothing  -> node n
    Left (Just _) -> do
      void . liftIO . atomically $ writeTVar (_activeNode ?ide) n
      node n
    Right Nothing -> pure () -- node n
    Right (Just diff) -> do
      log I $ "Node " <> Text.pack (show n) <>
              " received an update: " <> Text.pack (show diff)
--      pure ()
      trace <- liftIO (readTVarIO (_trace ?ide))
      void . liftIO . atomically $ writeTVar (_activeNode ?ide) (locKey (_focus trace))
      htmlTree (_states trace) diff
  where fetch :: STM (Maybe (Tree Int ()))
        fetch = do
          b <- (== n) <$> readTVar (_activeNode ?ide)
          if b then takeTMVar (_traceChanged ?ide) else retry
