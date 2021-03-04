{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}

module Redfin.IDE.Widget.Source
  ( sourceWidget
  ) where

import           Colog                     (HasLog (..), LogAction, Message,
                                            pattern D, pattern E, pattern I)
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
import           Data.Int                  (Int32)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy.Builder    as Text
import qualified Data.Text.Read            as Text
import           Prelude                   hiding (div, log, lookup, span)
import           Replica.VDOM.Render       as Render
import           Text.Read                 (readEither)

import           ISA.Types
import           ISA.Types.Instruction

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly              (Script, assemble)

-- | Display the assembly source code of the program
sourceWidget :: FilePath -> App [(CAddress, Instruction (Data Int32))]
sourceWidget fpath =
  div [classList [ ("box", True)
                 , ("sourceWidget", True)]]
    [ h3 [] [text "Source code"]
    , ol [classList [("listing", True)]]
      (map (li [] . (:[]) . text) src)
    , div [] [ Nothing <$ button [onClick] [text "Load from file"]
             , Just . getValue <$> input [value (Text.pack fpath)
                                         , placeholder "file.redfin"
                                         , onChange]]
    ] >>= \case
    Nothing -> do
      log I $ "Loading script from file " <> Text.pack fpath
      pure []
    Just fpath' -> sourceWidget fpath'
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) (_source ?ide)
