module Redfin.IDE
  ( ideWidget
  ) where

import           Colog                           (HasLog (..), LogAction,
                                                  Message, pattern D, pattern E,
                                                  pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                  hiding (defaultValue, id)
import qualified Concur.Replica.DOM.Events       as P
import           Control.Applicative             (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TSem
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative        as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                      as A
import           Data.Either                     (rights)
import           Data.Functor                    (void)
import           Data.Int                        (Int32)
import           Data.IntSet                     (IntSet)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
import qualified Data.Text.Read                  as Text
import           Prelude                         hiding (div, log, lookup, span)
import           Replica.VDOM.Render             as Render
import           Text.Read                       (readEither)

import           ISA.Backend.Symbolic.Zipper     hiding (_trace)
import qualified ISA.Backend.Symbolic.Zipper.Run as ISA
import qualified ISA.Example.Add                 as ExampleAdd
import qualified ISA.Example.Sum                 as ExampleSum
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic

import           Redfin.IDE.State
import           Redfin.IDE.Trace                (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget
import           Redfin.IDE.Widget.InitialState
import           Redfin.IDE.Widget.Source
import           Redfin.IDE.Widget.State
import           Redfin.IDE.Widget.Top
import           Redfin.IDE.Widget.Top.Examples
import           Redfin.IDE.Widget.Trace

import qualified Debug.Trace                     as Debugger

leftPane :: App a
leftPane = do
  section [classList [ ("pane", True)
                     , ("leftpane", True)
                     ]]
    [ div [classList [ ("leftpane-contents", True)]]
          [ sourceWidget ""
          , initStateWidget
          ]
    ]

elimEvent :: IDEEvent -> App IDEState
elimEvent = \case
  Proceed -> pure ?ide
  SourceChanged src -> pure $ ?ide { _source = src }
  SaveLoaded ide -> pure ide
  ExampleChanged ex -> do
    log D $ "Example changed to " <> Text.pack (show ex)
    ide' <- swapExample ex
    liftIO . atomically $ do
      writeTVar (_trace ide') emptyTrace
      writeTVar (_activeNode ide') (-1)
    pure ide'
  StepsChanged steps -> do
    log D $ "Steps changed to " <> Text.pack (show steps)
    liftIO . atomically $
      modifyTVar (_executor ?ide) (\e -> e {_executorSteps = steps})
    pure ?ide
  RunPressed -> do
    log D $ "Run pressed"
    executor <- liftIO (readTVarIO (_executor ?ide))
    env <- liftIO $
      evalEngine (ISA.runModelImpl (_executorSteps executor)) (_executorInitState executor)
    log D $ "Trace regenerated with " <> Text.pack (show (_executorSteps executor)) <> " steps"
    pure (?ide {_engine = env})
  InitStateChanged ctx -> do
    log D $ "Init state changed"
    liftIO . atomically $ do
      writeTVar (_trace ?ide) emptyTrace
      modifyTVar (_executor ?ide) (\e -> e {_executorInitState = ctx})
    pure ?ide
  ContraChanged contra -> do
    log D $ "Counterexample changed"
    liftIO . atomically $
      modifyTVar (_verifier ?ide) (\v -> v {_verifierContra = contra})
    pure ?ide

ideWidget :: App a
ideWidget = do
  event <- div [classList [("grid-container", True)]]
      [ Proceed <$ traceWidget
      , Proceed <$ stateWidget 0
      , Proceed <$ leftPane
      , Proceed <$ topPane
      , liftIO . atomically . readTQueue $ _events ?ide
      ]
  ide' <- elimEvent event
  let ?ide = ide' in
    ideWidget
