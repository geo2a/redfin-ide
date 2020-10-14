{-# LANGUAGE PatternSynonyms #-}

module Redfin.IDE.Widget.Source
  ( sourceWidget
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

import           ISA.Assembly              (Script, assemble)

-- | Display the assembly source code of the program
sourceWidget :: App a
sourceWidget =
  div [classList [ ("box", True)
                 , ("sourceCode", True)]]
    [ h3 [] [text "Source code"]
    , ol [] (map (li [] . (:[]) . text) src)
    ]
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) $ assemble (_source ?ide)
