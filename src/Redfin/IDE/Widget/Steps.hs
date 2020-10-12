{-# LANGUAGE PatternSynonyms #-}

module Redfin.IDE.Widget.Steps
  ( stepsWidget
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

-- | Specify the number of symbolic execution steps
stepsWidget :: Either Text Steps -> Widget HTML Steps
stepsWidget s = do
  let msg = either (const "") (Text.pack . show) s
  txt <- targetValue . target <$>
         div [classList [("stepsWidget", True)]]
             [ text ("Symbolic execution steps: ")
             , input [ placeholder msg
                     , value ""
                     , onChange, autofocus True]
             , either (const $ text "Error: invalid input") (const empty) s
             ]
  case (readEither . Text.unpack $ txt) of
    Left err -> stepsWidget (Left (Text.pack err))
    Right x  -> pure x
