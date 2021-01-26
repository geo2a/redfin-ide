{-# LANGUAGE PatternSynonyms #-}

module Redfin.IDE.Widget.State
  ( stateWidget
  ) where

import           Colog                      (pattern D, pattern E, HasLog (..),
                                             pattern I, LogAction, Message)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica             hiding (id)
import qualified Concur.Replica.DOM.Events  as P
import           Control.Applicative        (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative   as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                 as A
import           Data.Either                (rights)
import           Data.Functor               (void)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Builder     as Text
import qualified Data.Text.Read             as Text
import           Prelude                    hiding (div, log, lookup, span)
import           Replica.VDOM.Render        as Render
import           Text.Read                  (readEither)

import           ISA.Types
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace   hiding (htmlTrace)

import           Redfin.IDE.State
import           Redfin.IDE.Trace           (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly               (Script, assemble)

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
               [ div [classList [ ("rightpane-contents", True)]]
                   [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
                   , displayContext (lookup n trace)
                   , Right <$> (liftIO . atomically $ readTQueue (_activeNodeQueue ?ide))
                   ]
               ]
      case ev of
        Left _   -> widget n
        Right n' -> widget n'
