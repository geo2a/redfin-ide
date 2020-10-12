{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}
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
import           Data.Either                (rights)
import           Data.Functor               (void)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
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

parseSym :: Text -> Key -> Maybe (Key, Sym)
parseSym txt = Debugger.trace (Text.unpack txt) $ \case
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
  let relevant = Map.filterWithKey isRelevant (_bindings ctx)
  xs <- keyValsWidget . fmap (Text.pack . show) $ relevant
  let xs' = catMaybes $ map (uncurry (flip parseSym)) . Map.toList $ xs
  log I $ Text.pack (show xs')
  pure (MkContext (Map.union (Map.fromList xs') (_bindings ctx)) (SConst $ CBool True))
  where isRelevant :: Key -> a -> Bool
        isRelevant k _ = case k of
          Reg _  -> True
          Addr _ -> True
          F _    -> True
          _      -> False


keyValsWidget :: Map.Map Key Text -> App (Map.Map Key Text)
keyValsWidget ctx =
  go ctx
  where
    go ctx = do
      log D $ "Keys: " <> Text.pack (show $ Map.keys ctx)
      let inps = map keyInp (Map.assocs ctx)
      xs <- div [classList [("initState", True)]] .
              (h3 [] [text "Initial State"]:) . (:[]) $
              fmap (map (\(MkWithKey k v) -> (k,v))) .
              fmap rights . joinOrLast $
                 (map (fmap Right . div [] . (:[])) $ inps) ++
                 [(Left () <$ button [onClick] [text "Update"])]
      pure (Map.union (Map.fromList xs) ctx)

    keyInp :: (Key, Text) -> Widget HTML (WithKey Text)
    keyInp (key, v) =
      let keyTxt = Text.pack (show key) in
      text (keyTxt <> " = ") <|>
      input [ placeholder v
            , value v
            , MkWithKey key . targetValue . target <$> onChange
            ]
