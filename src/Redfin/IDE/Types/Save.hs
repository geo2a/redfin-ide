{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Redfin.IDE.Types.Save (saveIDE, loadIDE) where

import           Control.Exception

import           Colog                        (pattern D, HasLog (..),
                                               pattern I, LogAction (..),
                                               Message, Severity, WithLog,
                                               richMessageAction)
import           Control.Concurrent.STM
import           Control.Monad                (unless)
import           Control.Monad.IO.Class
import           Data.Aeson                   as JSON hiding (decode)
import           Data.Aeson.Encode.Pretty     as JSON
import           Data.Bifunctor               (second)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import           Data.Functor
import           Data.Int                     (Int32)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Generics
import           Prelude                      hiding (log)

import           ISA.Assembly
import           ISA.Types
import           ISA.Types.Instruction
import qualified ISA.Types.Instruction.Decode as ISA
import qualified ISA.Types.Instruction.Encode as ISA
import           ISA.Types.Symbolic
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.SMT
import           ISA.Types.Symbolic.Trace

import           Redfin.IDE.Types

-- | Save the state of the IDE for serialisation and persistent storing as a file
data Save =
  MkSave { _saveTrace              :: Trace Context
         , _saveDisplayUnreachabel :: Bool
         , _saveSteps              :: Steps
         , _saveTimeout            :: Int
         , _saveActiveNode         :: NodeId
         , _saveExample            :: Example
         , _saveInitState          :: Context
         , _saveSource             :: [(Address, InstructionCode)]
         } deriving (Show, Generic, ToJSON, FromJSON)

-- | Freeze the mutable variables from the IDE state
createSave :: IDEState -> IO Save
createSave ide = do
  trace <- atomically $ readTVar $ _trace ide
  pure $ MkSave trace
                (_displayUnreachableVal ide)
                (_stepsVal ide)
                (_timeoutVal ide)
                0
                (_activeExampleVal ide)
                (_activeInitStateVal ide)
                (map (second ISA.encode) $ _source ide)

-- | Create an IDE state based on a save
restoreSave :: Save -> IO IDEState
restoreSave save = do
  fresh <- emptyIDE
  atomically $
    writeTVar (_trace fresh) (_saveTrace save)
  pure $ fresh { _stepsVal = _saveSteps save
               , _timeoutVal = _saveTimeout save
               , _activeExampleVal = _saveExample save
               , _activeInitStateVal = _saveInitState save
               , _source = map (second (maybe (Instruction Halt) id . ISA.decode))
                               (_saveSource save)
               }

-- | Save the IDE state into a file
saveIDE :: FilePath -> IDEState -> IO (Either Text ())
saveIDE fpath ide =
  try (L.writeFile fpath . JSON.encode =<< createSave ide) >>= \case
    Left (e :: SomeException) -> pure (Left "I/O error: target file does not exist?")
    Right _ -> pure (Right ())

-- | Load an IDE state from the file
loadIDE :: FilePath -> IO (Either Text IDEState)
loadIDE fpath = do
  x <- try (B.readFile fpath)
  case x of
    Left (e :: SomeException) -> pure (Left "I/O error: target file does not exist?")
    Right txt ->
      case JSON.eitherDecodeStrict txt of
        Left err ->
          pure $ Left (Text.pack err)
        Right x ->
          Right <$> restoreSave x
  -- fmap JSON.eitherDecodeStrict (liftIO $ B.readFile fpath) >>= \case
  --   Left err -> pure $ Left (Text.pack err)
  --   Right x ->
  --     Right <$> restoreSave x
