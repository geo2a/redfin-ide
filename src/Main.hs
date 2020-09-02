module Main where

import           Concur.Replica
import           Control.Applicative           ((<|>))
import           Control.Monad                 (forever)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Map.Strict               as Map
import           Data.Text                     hiding (index)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Middleware.Static as Static
import           Network.WebSockets            (defaultConnectionOptions)
import           Text.Read                     (readMaybe)

import           IDE

static :: Wai.Middleware
static =
  Static.staticPolicy $
    Static.only [("styles/custom.css", "styles/custom.css")]

index :: HTML
index =
  [ VLeaf "!doctype" (fl [("html", ABool True)]) Nothing,
    VNode
      "html"
      mempty
      Nothing
      [ VNode
          "head"
          mempty
          Nothing
          [ VLeaf "meta" (fl [("charset", AText "utf-8")]) Nothing,
            VNode "title" mempty Nothing [VText "Redfin IDE"],
            VLeaf
              "meta"
              ( fl
                  [ ("name", AText "viewport"),
                    ("content", AText "width=device-width, initial-scale=1")
                  ]
              )
              Nothing,
            VLeaf
              "link"
              ( fl
                  [ ("href", AText "styles/custom.css"),
                    ("rel", AText "stylesheet")
                  ]
              )
              Nothing
          ],
        VNode
          "body"
          mempty
          Nothing
          [ VNode
              "script"
              (fl [("language", AText "javascript")])
              Nothing
              [VRawText $ decodeUtf8 clientDriver]
          ]
      ]
  ]
  where
    fl :: [(Text, Attr)] -> Attrs
    fl = Map.fromList

main :: IO ()
main = run
  8080
  index
  defaultConnectionOptions
  static
  (\_ -> ide)
