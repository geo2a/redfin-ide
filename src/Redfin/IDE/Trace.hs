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
  ev <- a [classList [ ("node", True)
                  , ("interactive", True)
                  , ("expanded", True)
                  , ("reachable", isReachable ctx)
                  , ("hidden", (not $ isReachable ctx) && (not $ _displayUnreachableVal ?ide))
                  ]
             , id ("node" <> (Text.pack . show $ n))
             , href "#"
             , Right <$> onClick
             ]
             [ text (Text.pack . show $ n)]
  case ev of
    Left e  -> node args
    Right e -> do
      log D $ "Click on node " <> Text.pack (show n)
      liftIO . atomically $
        writeTQueue (_activeNodeQueue ?ide) n
      log D $ "Active node changed to " <> Text.pack (show n)
      node args

htmlTree :: Tree.Tree (Context, Int) -> App a
htmlTree (Tree.Node (ctx,i) []) = li [] [node (ctx, i)]
htmlTree (Tree.Node n ns)
    = li [] [node n, ul [] (map htmlTree ns)]

htmlTrace :: Trace Context -> App a
htmlTrace (Trace tree) =
  div [classList [("tree", True)]]
  [ ul [] [htmlTree . enumTree . fmap _nodeBody $ tree]]

-- <div class="tree">
-- 	<ul>
--   <li>
--     <a href="#">Parent</a>
--     <ul>
--       <li>
--         <a href="#">Child</a>
--         <ul>
--           <li>
--             <a href="#">Grand Child</a>
--           </li><li>
--             <a href="#">Grand Child</a>
--             <ul>
--               <li>
--                 <a href="#">Grand Child</a>
--               </li><li>
--                 <a href="#">Grand Child</a>
--               </li>
--             </ul>
--           </li>
--         </ul>
--       </li><li>
--         <a href="#">Child</a>
--         <ul>
--           <li>
--             <a href="#">Grand Child</a>
--             <ul>
--               <li>
--                 <a href="#">Grand Grand Child</a>
--               </li>
--             </ul>
--           </li><li>
--             <a href="#">Grand Child</a>
--           </li>
--         </ul>
--       </li>
--     </ul>
--   </li>
-- </ul>
