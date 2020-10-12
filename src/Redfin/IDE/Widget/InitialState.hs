{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
module Redfin.IDE.Widget.InitialState
  ( initStateWidget
  ) where

import           Colog                     (pattern D, pattern E, HasLog (..),
                                            pattern I, LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica            hiding (id)
import qualified Concur.Replica.DOM.Events as P
import           Control.Applicative       (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative  as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                as A
import           Data.Either               (rights)
import           Data.Functor              (void)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy.Builder    as Text
import qualified Data.Text.Read            as Text
import           Prelude                   hiding (div, log, lookup, span)
import           Replica.VDOM.Render       as Render
import           Text.Read                 (readEither)

import           ISA.Types

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

initStateWidget :: App a
initStateWidget = do
  xs <- keyValsWidget Map.empty
  -- log I $ Text.pack (show xs)
  initStateWidget

keyValsWidget :: Map.Map Key Text -> App (Map.Map Key Text)
keyValsWidget st =
  go st
  where
    go st = do
      xs <- div [classList [("initState", True)]] . (:[]) $
        fmap rights . joinOrLast $
               [ Right <$> div [] [keyInp (Reg R0)]
               , Right <$> div [] [keyInp (Reg R1)]
               , Left () <$ button [onClick] [text "Initialise"]
               ]
      log I $ Text.pack (show xs)
      go st

    keyInp :: Key -> Widget HTML (WithKey Text)
    keyInp key =
      let keyTxt = Text.pack (show key) in
      text (keyTxt <> " = ") <|>
      input [ placeholder (Text.pack (show key))
            , value ""
            , MkWithKey key . targetValue . target <$> onChange
            ]
