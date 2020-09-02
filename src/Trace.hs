module Trace where

import           Concur.Replica
import           Control.Applicative        ((<|>))
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Tree                  as Tree
import           Prelude                    hiding (div)

import           ISA.Types
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace

mapVText :: (Text -> Text) -> VDOM -> VDOM
mapVText f s = case s of
  VNode "span" x y [ VRawText txt ] -> VNode "span" x y [ VRawText (f txt) ]
  -- VNode "span" x y [ VNode "span" a b [VRawText txt] ] ->
  --   VNode "span" x y [ VNode "span" a b [VRawText (f txt)] ]
  _                                 -> s

transformHTML :: (VDOM -> Bool) -> (VDOM -> VDOM) -> [VDOM] -> [VDOM]
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

-- indentInit :: [VDOM] -> [VDOM]
-- indentInit []     = []
-- indentInit (s:ss) = mapVText (" ├╴" <>) s : map (mapVText (" │ " <>)) ss
indentInit :: [VDOM] -> [VDOM]
indentInit []     = []
indentInit (s:ss) =
  mapVText (" ├╴" <>) s : transformHTML (const True) (mapVText (" │ " <>)) ss

-- indentLast :: [VDOM] -> [VDOM]
-- indentLast []     = []
-- indentLast (s:ss) = mapVText (" └╴" <>) s : map (mapVText ("   " <>)) ss

indentLast :: [VDOM] -> [VDOM]
indentLast []     = []
indentLast (s:ss) =
  mapVText (" └╴" <>) s : transformHTML (const True) (mapVText ("   " <>)) ss

-- indentChildren :: [[Text]] -> [[Text]]
-- indentChildren [] = []
-- indentChildren ns = map indentInit (init ns) ++ [indentLast (last ns)]

indentChildren :: [HTML] -> [HTML]
indentChildren [] = []
indentChildren ns = map indentInit (init ns) <> [indentLast (last ns)]

appLast :: [Text] -> Text -> [Text]
appLast ss s = init ss <> [last ss <> s]

escapeBrackets :: Text -> Text
escapeBrackets = Text.concatMap fixBrack
  where
    fixBrack '<' = "&lt;"
    fixBrack '>' = "&gt;"
    fixBrack c   = Text.singleton c

htmlNode :: (Context, Int) -> HTML
htmlNode (ctx, n) =
  [ VNode "span"
          (Map.fromList [ ("id", AText ("node" <> (Text.pack . show $ n)))
                        , ("class", AText "node interactive expanded")
                        ])
          Nothing
          [ VRawText $ Text.pack . show $ n ]
  ]

childNode :: Int -> [HTML] -> HTML
childNode n children =
  [ VNode "span"
          (Map.fromList [ ("id", AText ("children_node" <> (Text.pack . show $ n)))
                        , ("class", AText "child shown")
                        ])
          Nothing
          (concat $ indentChildren children)
  ]

enumTree :: Tree.Tree a -> Tree.Tree (a,Int)
enumTree = flip evalState 0 . traverse count
  where
    count :: a -> State Int (a,Int)
    count a = do
      i <- get; put (i+1)
      return (a,i)

showTreeHtml' :: Tree.Tree (Context, Int) -> HTML
showTreeHtml' (Tree.Node (ctx,i) []) = htmlNode (ctx, i)
showTreeHtml' (Tree.Node n ns)
    = htmlNode n <>
      childNode (snd n) (map showTreeHtml' ns)
    -- ++ appLast (concat (indentChildren (map showTreeHtml' ns))) "</span>"

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

htmlTrace :: Trace Context -> HTML
htmlTrace (Trace tree) =
  transformHTML isLeaf (mapVText (Text.cons '\n')) .
  showTreeHtml' . enumTree . fmap nodeBody $ tree
  where isLeaf :: VDOM -> Bool
        isLeaf node = case node of
          VNode "span" attrs _ _ ->
            case Text.isPrefixOf <$> Just "node" <*> getClass node of
              Just _  -> True
              Nothing -> False
          _ -> False
