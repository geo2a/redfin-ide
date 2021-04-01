{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redfin.IDE.Types.Save (saveIDE, loadIDE) where

import           Control.Exception

import           Colog                        (HasLog (..), LogAction (..),
                                               Message, Severity, WithLog,
                                               pattern D, pattern I,
                                               richMessageAction)
import           Control.Concurrent.STM
import           Control.Monad                (unless)
import           Control.Monad.IO.Class
import           Data.Aeson                   as JSON hiding (Value, decode)
import           Data.Aeson.Encode.Pretty     as JSON
import           Data.Bifunctor               (second)
import qualified Data.ByteString.Lazy         as LB
import           Data.Functor
import           Data.Int                     (Int32)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Generics
import           Prelude                      hiding (log)

import           ISA.Assembly
import           ISA.Backend.Symbolic.Zipper  hiding (_trace)
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction
import qualified ISA.Types.Instruction.Decode as ISA
import qualified ISA.Types.Instruction.Encode as ISA
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Address
import           ISA.Types.Tree

import           Redfin.IDE.Types

-- | Save the state of the IDE for serialisation and persistent storing as a file
data Save =
  MkSave { _saveEngine     :: (Trace, Int, Int, Maybe Int)
         , _saveSteps      :: Steps
         , _saveActiveNode :: NodeId
         , _saveInitState  :: Context Sym
         , _saveSource     :: [(CAddress, InstructionCode)]
         } deriving (Generic, ToJSON, FromJSON)

-- | Freeze the mutable variables from the IDE state
createSave :: IDEState -> IO Save
createSave ide = do
  (trace, statesCount, varCount, fuel) <- (,,,) <$>
    readTVarIO (_trace ide) <*>
    readTVarIO (_statesCount $ _engine ide) <*>
    readTVarIO (_varCount $ _engine ide) <*>
    pure (_simplifyFuel $ _engine ide)
  executor <- readTVarIO $ _executor ide
  pure $ MkSave (trace, statesCount, varCount, fuel)
                (_executorSteps executor)
                0
                (_executorInitState executor)
                (map (second ISA.encode) $ _source ide)

-- | Create an IDE state based on a save
restoreSave :: Save -> IO IDEState
restoreSave save = do
  fresh <- emptyIDE
  let (trace, statesCount, varCount, fuel) = _saveEngine save
  atomically $ do
    writeTVar (_executor fresh) (defaultValue { _executorSteps = _saveSteps save
                                              , _executorInitState = _saveInitState save
                                              })
    writeTVar (_trace fresh) trace
    writeTVar (_statesCount . _engine $ fresh) statesCount
    writeTVar (_varCount . _engine $ fresh) varCount
  pure $ fresh { _activeExampleVal = None
               , _source =
                 map (second (maybe (Instruction (Halt @Value)) id
                               . ISA.decode))
                 (_saveSource save)
               , _engine = (_engine fresh) {_simplifyFuel = fuel}
               }

-- | Save the IDE state into a file
saveIDE :: FilePath -> IDEState -> IO (Either Text ())
saveIDE fpath ide =
  try (LB.writeFile fpath . JSON.encode =<< createSave ide) >>= \case
    Left (e :: SomeException) -> pure (Left "I/O error: target file does not exist?")
    Right _ -> pure (Right ())

-- | Load an IDE state from the file
loadIDE :: FilePath -> IO (Either Text IDEState)
loadIDE fpath = do
  x <- try (LB.readFile fpath)
  case x of
    Left (e :: SomeException) -> pure (Left "I/O error: target file does not exist?")
    Right txt ->
      case JSON.eitherDecode txt of
        Left err ->
          pure $ Left (Text.pack err)
        Right x ->
          Right <$> restoreSave x
