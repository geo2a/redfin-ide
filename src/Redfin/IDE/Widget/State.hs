{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}

module Redfin.IDE.Widget.State
  ( stateWidget
  ) where

import           Colog                           (HasLog (..), LogAction,
                                                  Message, pattern D, pattern E,
                                                  pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                  hiding (id)
import qualified Concur.Replica.DOM.Events       as P
import           Control.Applicative             (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative        as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                      as A
import           Data.Either                     (rights)
import           Data.Functor                    (void)
import qualified Data.IntMap                     as IntMap
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
import qualified Data.Text.Read                  as Text
import           Prelude                         hiding (div, log, lookup, span)
import           Replica.VDOM.Render             as Render
import           Text.Read                       (readEither)

import           ISA.Backend.Symbolic.Zipper     hiding (_trace)
import           ISA.Backend.Symbolic.Zipper.Run
import           ISA.Types
import           ISA.Types.Context               hiding (Context)
import           ISA.Types.Tree                  hiding (Cxt (..))

import           Redfin.IDE.State
import           Redfin.IDE.Trace                (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly                    (Script, assemble)

stateWidget :: App a
stateWidget = do
  widget (_activeNodeVal ?ide)
  where
    widget n = do
      log D $ "displaying state for node " <> Text.pack (show n)
      trace <- liftIO $ readTVarIO (_trace ?ide)

      ev <-  section [classList [ ("pane", True)
                   , ("rightpane", True)
                   ]]
               [ div [classList [ ("box", True)]]
                   [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
                   , Just . Left <$> button [onClick] [text "Continue"]
                   , displayContext (IntMap.lookup n (_states trace))
--                   , Just . Right <$> (liftIO . atomically $ takeTMVar (_activeNodeQueue ?ide))
                   ]
               ]
      case ev of
        Nothing       -> widget n
        Just (Left _) -> continueFromHere n *> widget n
--        Just (Right n') -> widget n'

continueFromHere :: Int -> App ()
continueFromHere state = do
  trace <- liftIO (readTVarIO (_trace ?ide))
  case findLoc state (_layout trace) of
    Nothing -> do
      log E $ "no state with id " <> Text.pack (show state)
      pure ()
    Just loc -> do
      liftIO . atomically $ writeTVar (_trace ?ide) (trace { _focus = loc })
--      s <- getFocused
      liftIO $ continueEngine (runModelImpl 3) (_engine ?ide)
