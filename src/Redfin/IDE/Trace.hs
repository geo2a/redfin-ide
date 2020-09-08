module Redfin.IDE.Trace where

import           Concur.Core
import           Concur.Replica
import qualified Concur.Replica.DOM.Events  as P
import           Control.Applicative        ((<|>))
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Tree                  as Tree
import           Prelude                    hiding (div, id, span)

import           ISA.Types
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace

mapVText :: (Text -> Text) -> VDOM -> VDOM
mapVText f s = case s of
  VNode "span" x y [ VRawText txt ] -> VNode "span" x y [ VRawText (f txt) ]
  VNode "span" x y [ VText txt ]    -> VNode "span" x y [ VText (f txt) ]
  _                                 -> s

transformHTML :: (VDOM -> Bool) -> (VDOM -> VDOM) -> HTML -> HTML
transformHTML selector f = map transform
  where transform node =
          if selector node
          then let node' = f node in
               case node' of
                 VNode n as ns cs -> VNode n as ns (transformHTML selector f cs)
                 _ -> node'
          else case node of
            VNode n as ns cs ->
              VNode n as ns (transformHTML selector f cs)
            _ -> node

indentInit :: HTML -> HTML
indentInit []     = []
indentInit (s:ss) =
  mapVText (" ├╴" <>) s : transformHTML (const True) (mapVText (" │ " <>)) ss

indentLast :: HTML -> HTML
indentLast []     = []
indentLast (s:ss) =
  mapVText (" └╴" <>) s : transformHTML (const True) (mapVText ("   " <>)) ss

indentChildren :: [Widget HTML a] -> [Widget HTML a]
indentChildren [] = []
indentChildren ns = map (mapView indentInit) (init ns) <> [mapView indentLast (last ns)]

node :: (Context, Int) -> Widget HTML NodeId
node args@(ctx, n) = do
  ev <- span [classList [ ("node", True)
                  , ("interactive", True)
                  , ("expanded", True)
                  ]
       , id ("node" <> (Text.pack . show $ n))
       , Right <$> onMouseDown
       ]
       [text $ Text.pack . show $ n]
  case ev of
    Left e  -> node args
    Right e -> pure n

children :: Int -> [Widget HTML a] -> Widget HTML a
children n children =
  span [classList [ ("child", True)
                  , ("shown", True)
                  ]
       , id ("children_node" <> (Text.pack . show $ n))
       ]
       (indentChildren children)

enumTree :: Tree.Tree a -> Tree.Tree (a,Int)
enumTree = flip evalState 0 . traverse count
  where
    count :: a -> State Int (a,Int)
    count a = do
      i <- get; put (i+1)
      return (a,i)

showTreeHtml' :: Tree.Tree (Context, Int) -> Widget HTML NodeId
showTreeHtml' (Tree.Node (ctx,i) []) = node (ctx, i)
showTreeHtml' (Tree.Node n ns)
    = node n <|>
      children (snd n) (map showTreeHtml' ns)

getClass :: VDOM -> Maybe Text
getClass = \case
  VNode _ attrs _ _ ->
    Map.lookup "class" attrs >>= getClass'
  _ -> Nothing

getClass' :: Attr -> Maybe Text
getClass' = \case
  AText txt -> Just txt
  AMap attrs -> Map.lookup "class" attrs >>= getClass'
  _ -> Nothing

htmlTrace :: Trace Context -> Widget HTML NodeId
htmlTrace (Trace tree) =
  mapView (transformHTML isLeaf (mapVText (Text.cons '\n'))) .
  showTreeHtml' . enumTree . fmap nodeBody $ tree
  where isLeaf :: VDOM -> Bool
        isLeaf node = case node of
          VNode "span" attrs _ _ ->
            case Text.isPrefixOf <$> Just "node" <*> getClass node of
              Just _  -> True
              Nothing -> False
          _ -> False

--------------------------------------------------------------------------------
renderContext :: Context -> Text
renderContext = undefined