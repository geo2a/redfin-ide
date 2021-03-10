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
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
import qualified Data.Text.Read                  as Text
import           Prelude                         hiding (div, log, lookup, span)
import           Replica.VDOM.Render             as Render
import           Text.Read                       (readEither)

import           ISA.Backend.Symbolic.Zipper     (_focus, _layout, _states,
                                                  continueEngine, down,
                                                  growTrace)
import           ISA.Backend.Symbolic.Zipper.Run
import           ISA.Types
import           ISA.Types.Context               hiding (Context)
import           ISA.Types.Key
import           ISA.Types.Prop
import           ISA.Types.Tree                  hiding (Cxt (..), down)
import           ISA.Types.ZeroOneTwo

import           Redfin.IDE.State
import           Redfin.IDE.Trace                (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly                    (Script, assemble)

stateWidget :: Int -> App a
stateWidget n = do
  log D $ "displaying state for node " <> Text.pack (show n)
  trace <- liftIO $ readTVarIO (_trace ?ide)
  ev <- section [classList [ ("pane", True)
                           , ("rightpane", True)
                           ]]
        [ div [classList [ ("box", True)]]
            [
              h3 [] [ text $ "State " <> Text.pack (show n) <> ": "]
            , Just . Left <$> button [onClick] [text "Continue"]
            , displayContext (IntMap.lookup n (_states trace))
            , Just . Right <$> (liftIO . atomically $ checkActiveNode)
            ]
        ]
  case ev of
    Nothing         -> stateWidget n
    Just (Left _)   -> do
      let halted = maybe False toBool (getBinding (F Halted) =<< IntMap.lookup n (_states trace))
      when (Prelude.not halted) $ do
        diff <- continueFrom n
        liftIO . atomically $ orElse (putTMVar (_traceChanged ?ide) diff)
                                     (void $ swapTMVar (_traceChanged ?ide) diff)
      stateWidget n
    Just (Right n') -> stateWidget n'
  where checkActiveNode = do
          n' <- readTVar (_activeNode ?ide)
          if n' /= n then pure n' else retry

continueFrom :: Int -> App (Maybe (Tree Int ()))
continueFrom state = do
  trace <- liftIO (readTVarIO (_trace ?ide))
  case findLoc state (_layout trace) of
    Nothing -> do
      log E $ "no state with id " <> Text.pack (show state)
      pure Nothing
    Just loc -> do
      liftIO . atomically $ writeTVar (_trace ?ide) (trace { _focus = loc })
      s <- case IntMap.lookup state (_states trace) of
                Nothing -> log E ("No such state " <> Text.pack (show state)) >> undefined
                Just z  -> pure z
      liftIO $ continueEngine (runModelImpl 1) (_engine ?ide)
      trace <- liftIO . readTVarIO $ (_trace ?ide)
--      log E $ Text.pack (unlines $ draw (_layout trace))
      pure . Just $ travel (_focus trace) (up *> getTree)
