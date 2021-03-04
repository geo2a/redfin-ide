{-# LANGUAGE PatternSynonyms #-}

module Redfin.IDE.Widget.State
  ( stateWidget
  ) where

import           Colog                       (HasLog (..), LogAction, Message,
                                              pattern D, pattern E, pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica              hiding (id)
import qualified Concur.Replica.DOM.Events   as P
import           Control.Applicative         (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative    as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                  as A
import           Data.Either                 (rights)
import           Data.Functor                (void)
import qualified Data.IntMap                 as IntMap
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy.Builder      as Text
import qualified Data.Text.Read              as Text
import           Prelude                     hiding (div, log, lookup, span)
import           Replica.VDOM.Render         as Render
import           Text.Read                   (readEither)

import           ISA.Backend.Symbolic.Zipper hiding (_trace)
import           ISA.Types
import           ISA.Types.Context           hiding (Context)

import           Redfin.IDE.State
import           Redfin.IDE.Trace            (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly                (Script, assemble)

stateWidget :: App a
stateWidget = do
  widget 0
  where
    widget n = do
      log D $ "displaying state for node " <> Text.pack (show n)
      trace <- liftIO $ readTVarIO (_trace ?ide)

      ev <-  section [classList [ ("pane", True)
                   , ("rightpane", True)
                   ]]
               [ div [classList [ ("box", True)]]
                   [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
                   , displayContext (IntMap.lookup n (_states trace))
                   , Right <$> (liftIO . atomically $ readTQueue (_activeNodeQueue ?ide))
                   ]
               ]
      case ev of
        Left _   -> widget n
        Right n' -> widget n'
