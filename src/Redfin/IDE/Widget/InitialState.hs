{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module Redfin.IDE.Widget.InitialState
  ( initStateWidget
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
import           Data.Either                (lefts, rights)
import           Data.Functor               (void)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Monoid                (First (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Builder     as Text
import qualified Data.Text.Read             as Text
import           Debug.Trace                as Debugger
import           Prelude                    hiding (div, log, lookup, span)
import           Replica.VDOM.Render        as Render
import           Text.Read                  (readEither, readMaybe)

import           ISA.Types
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

parseKey :: String -> Maybe Key
parseKey key =
   getFirst . mconcat . map First $ [ Reg  <$> readMaybe key
                                    , F    <$> readMaybe key
                                    , Addr <$> readMaybe key]

parseSym :: Text -> Key -> Maybe (Key, Sym)
parseSym txt = \case
  Reg r -> (Reg r,) <$>
    case readMaybe (Text.unpack txt) of
      Just v  -> Just . SConst . CInt32 $ v
      Nothing -> Just $ SAny txt
  Addr a ->  (Addr a,) <$>
    case readMaybe (Text.unpack txt) of
      Just v  -> Just . SConst . CInt32 $ v
      Nothing -> Just $ SAny txt
  F f -> (F f,) . SConst . CBool <$> readMaybe (Text.unpack txt)
  IC -> error "parseSym: not implemented for key IC"
  IR -> error "parseSym: not implemented for key IR"
  Prog _ -> error "parseSym: not implemented for key Prog"

initStateWidget :: Context -> App Context
initStateWidget ctx = do
  let relevant = fmap (Text.pack . show) $ Map.filterWithKey isRelevant (_bindings ctx)
  buffer <- liftIO . atomically $ newTVar relevant
  xs <- keyValsWidget buffer
  let xs' = catMaybes . map (uncurry (flip parseSym)) . Map.toList $ xs
  log I $ Text.pack (show xs')
  pure (MkContext (Map.union (Map.fromList xs') (_bindings ctx))
                  (SConst $ CBool True) (_constraints ctx) Nothing)
  where isRelevant :: Key -> a -> Bool
        isRelevant k _ = case k of
          Reg _  -> True
          Addr _ -> True
          F _    -> True
          _      -> False

keyValsWidget :: TVar (Map.Map Key Text) -> App (Map.Map Key Text)
keyValsWidget buffer = do
  ctx <- liftIO $ readTVarIO buffer
  let inps = map (keyInp buffer) (Map.assocs ctx)
  t <- div [classList [ ("box", True)]]
             [ h3 [] [text "Initial State"]
             , div [classList [("initState", True)]] $
                   map (Just <$>) inps ++
                   [ addBindingWidget buffer
                   , Nothing <$ button [onClick] [text "Update"]
                   ]]
  case t of
    Just _  -> keyValsWidget buffer
    Nothing -> liftIO $ readTVarIO buffer


keyInp :: TVar (Map.Map Key Text) -> (Key, Text) -> Widget HTML a
keyInp buffer (key, v) = do
  let keyTxt = Text.pack (show key)
  new <- orr
    [ span [classList [("initKey", True)]] [text keyTxt]
    , span [classList [("initEq", True)]] [text " = "]
    , span [classList [("initVal", True)]]
          [input [ placeholder v
                 , value v
                 , targetValue . target <$> onChange
                 ]
          ]]
  liftIO . atomically $ modifyTVar' buffer (\ctx -> Map.insert key new ctx)
  keyInp buffer (key, new)

addBindingWidget :: TVar (Map.Map Key Text) -> Widget HTML (Maybe (WithKey Text))
addBindingWidget buffer = do
  let tip = "Type a key: register, address or flag"
  keyTxt <- orr [ span [classList [("initKey", True)]]
                [tooltipped tip $ input [ targetValue . target <$> onChange
                                        , placeholder "New key"
                                        ]
                ]
             , span [classList [("initEq", True)]] [text " = "]
             , span [classList [("initVal", True)]]
                 [input [ disabled True
                        , placeholder "New value"
                        ]
                 ]
             ]
  value <- orr [ span [classList [("initKey", True)]]
                   [input [ disabled True, placeholder keyTxt]
                   ]
               , span [classList [("initEq", True)]] [text " = "]
               , span [classList [("initVal", True)]]
                     [input [ targetValue . target <$> onChange
                            , placeholder ""
                            ]
                     ]
               ]
  case parseKey (Text.unpack keyTxt) of
    Nothing -> addBindingWidget buffer
    Just key -> do
      liftIO . atomically $ modifyTVar' buffer (\ctx -> Map.insert key value ctx)
      addBindingWidget buffer
