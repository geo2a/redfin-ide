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
import           Data.List                  (union)
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
import           ISA.Types.Symbolic.Parser

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

parseKey :: String -> Maybe Key
parseKey key =
   getFirst . mconcat . map First $ [ Reg  <$> readMaybe key
                                    , F    <$> readMaybe key
                                    , Addr <$> readMaybe key]

parseValue :: Text -> Key -> Maybe (Key, Sym)
parseValue txt = \case
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

initStateWidget :: Context -> App a
initStateWidget ctx = do
  let relevant = fmap (Text.pack . show) $ Map.filterWithKey isRelevant (_bindings ctx)
      cs = Map.fromList (_constraints ctx)
  bufferBindings <- liftIO $ newTVarIO relevant
  bufferConstraints <- liftIO $ newTVarIO cs
  xs <- orr [ Left <$> keyValsWidget bufferBindings
            , Right <$> constrWidget bufferConstraints]
  let bindings = either id (const relevant) xs
      constraints = either (const cs) id xs
  let xs' = catMaybes . map (uncurry (flip parseValue)) . Map.toList $ bindings
  -- log I $ Text.pack (show xs')
  let newInitState =
        (MkContext (Map.union (Map.fromList xs') (_bindings ctx))
                  (SConst $ CBool True)
                  (Map.assocs constraints)
                  Nothing)
  when (newInitState /= (_activeInitStateVal ?ide)) $ do
    liftIO . atomically $ putTMVar (_activeInitState ?ide) newInitState
    pure ()
  initStateWidget newInitState
  where isRelevant :: Key -> a -> Bool
        isRelevant k _ = case k of
          Reg _  -> True
          Addr _ -> True
          F _    -> True
          _      -> False

keyValsWidget :: TVar (Map.Map Key Text) -> App (Map.Map Key Text)
keyValsWidget buffer = do
  ctx <- liftIO $ readTVarIO buffer
  let inps = case (_activeExampleVal ?ide) of
               None -> []
               _    -> map (keyInp buffer) (Map.assocs ctx)
                    ++ [addBindingWidget buffer]
  div [classList [ ("box", True)]]
             [ h3 [] [text "Initial State"]
             , div [classList [("initState", True)]] $
                   inps ++
                   [void $ button [onClick] [text "Update"]
                   ]]
  liftIO $ readTVarIO buffer


keyInp :: TVar (Map.Map Key Text) -> (Key, Text) -> Widget HTML ()
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

addBindingWidget :: TVar (Map.Map Key Text) -> Widget HTML ()
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
                            , autofocus True
                            ]
                     ]
               ]
  case parseKey (Text.unpack keyTxt) of
    Nothing -> addBindingWidget buffer
    Just key -> do
      liftIO . atomically $ modifyTVar' buffer (\ctx -> Map.insert key value ctx)

--------------------------------------------------------------------------------

constrWidget :: TVar (Map.Map Text Sym) -> App (Map.Map Text Sym)
constrWidget buffer = do
  cs <- liftIO $ readTVarIO buffer
  let inps = case (_activeExampleVal ?ide) of
               None -> []
               _    -> (map (constrInp buffer) (Map.assocs cs))
                    ++ [addConstraintWidget buffer]
  div [classList [ ("box", True)]]
             [ h3 [] [text "Constraints"]
             , div [classList [("initState", True)]] $
               inps ++
               [void $ button [onClick] [text "Update"]
              ]]
  liftIO . atomically $ readTVar buffer

constrInp :: TVar (Map.Map Text Sym) -> (Text, Sym) -> Widget HTML ()
constrInp buffer (name, expr) = do
  let exprTxt = Text.pack $ show expr
  new <- orr
    [ Nothing <$ span [classList [("initKey", True)]]
                      [a [classList [("close",True)], onClick] [], text name]
    , span [classList [("initEq", True)]] [text " : "]
    , Just <$> span [classList [("initVal", True)]]
                    [input [ placeholder exprTxt
                           , value exprTxt
                           , targetValue . target <$> onChange
                           ]
          ]]
  case parseSym (Text.unpack name) <$> new of
      Just (Left err) -> text err
      Just (Right sym) -> do
        liftIO . atomically $
          modifyTVar' buffer (\ctx -> Map.insert name sym ctx)
      Nothing -> do
        liftIO . atomically $ do
          modifyTVar' buffer (\ctx -> Map.delete name ctx)


addConstraintWidget :: TVar (Map.Map Text Sym) -> Widget HTML ()
addConstraintWidget  buffer = do
  name <- orr [ span [classList [("initKey", True)]]
                [ input [ targetValue . target <$> onChange
                        , placeholder "Constraint name"
                        ]
                ]
             , span [classList [("initEq", True)]] [text " : "]
             , span [classList [("initVal", True)]]
                 [input [ disabled True
                        , placeholder "Symbolic expression"
                        ]
                 ]
             ]
  expr <- orr [ span [classList [("initKey", True)]]
                   [input [ disabled True, placeholder name]
                   ]
               , span [classList [("initEq", True)]] [text " : "]
               , span [classList [("initVal", True)]]
                     [input [ targetValue . target <$> onChange
                            , placeholder ""
                            , autofocus True
                            ]
                     ]
               ]
  case parseSym (Text.unpack name) expr of
    Left err -> orr [text err, addConstraintWidget buffer]
    Right sym ->
      liftIO . atomically $ modifyTVar' buffer (\ctx -> Map.insert name sym ctx)
