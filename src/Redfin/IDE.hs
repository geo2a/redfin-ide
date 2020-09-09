{-# LANGUAGE ScopedTypeVariables #-}

module Redfin.IDE
  ( ide
  ) where

import           Concur.Core
import           Concur.Replica               hiding (id)
import qualified Concur.Replica.DOM.Events    as P
import           Control.Applicative          ((<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Aeson                   as A
import           Data.Functor                 (void)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy.Builder       as Text
import qualified Data.Text.Read               as Text
import           Prelude                      hiding (div, lookup)
import           Replica.VDOM.Render          as Render

import           ISA.Assembly                 (assemble)
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace     hiding (htmlTrace)

import           Redfin.IDE.State
import           Redfin.IDE.Trace             (htmlTrace)

import qualified ISA.Example.Add              as Example

-- | Either-like datatype for tracking new/old content of tags
data Contents a b = Old a
                  | New b
                  deriving (Show, Eq)

type Steps = Int

symexecTrace :: Steps -> Trace Context
symexecTrace steps = Example.symexecTrace steps

topWidget :: TChan Steps -> Widget HTML a
topWidget stepsVar = do
  steps <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [controlWidget]
  liftIO . atomically $ writeTChan stepsVar steps
  liftIO $ print steps
  topWidget stepsVar

controlWidget :: Widget HTML Steps
controlWidget =
  read . Text.unpack . targetValue . target <$>
    div []
      [ text "Symbolic execution steps: "
      , input [onChange]
      ]

sourceWidget :: Widget HTML a
sourceWidget =
  div [classList [ ("pane", True)
                 , ("leftpane", True)
                 ]
      ]
      [ol [classList [("sourceCode", True)]] (map (li [] . (:[]) . text) src)]
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) $ assemble Example.addLowLevel

traceWidget :: TVar (Trace Context)
            -> Steps -> TChan Steps
            -> TChan NodeId -> Widget HTML a
traceWidget traceVar steps stepsVar nodeIdVar = do
  contents <- Old <$> widget steps
          <|> New <$> (liftIO . atomically $ readTChan stepsVar)
  case contents of
    Old _      -> traceWidget traceVar steps stepsVar nodeIdVar
    New steps' -> traceWidget traceVar steps' stepsVar nodeIdVar
  where widget steps = do
          let trace = symexecTrace steps
          n <- div [classList [ ("pane", True)
                              , ("middlepane", True)
                              ]
                   ]
               [pre [] [htmlTrace trace]]
          liftIO . atomically $ writeTVar traceVar trace
          liftIO . atomically $ writeTChan nodeIdVar n


stateWidget :: TVar (Trace Context) -> NodeId -> TChan NodeId -> Widget HTML a
stateWidget traceVar n nodeIdChan = do
  contents <- Old <$> widget
          <|> New <$> (liftIO . atomically $ readTChan nodeIdChan)
  case contents of
    Old _  -> stateWidget traceVar n nodeIdChan
    New n' -> stateWidget traceVar n' nodeIdChan
  where
    widget :: Widget HTML NodeId
    widget = do
      trace <- liftIO . atomically $ readTVar traceVar
      div [classList [ ("pane", True)
                     , ("rightpane", True)
                     ]
          ]
        [displayContext (lookup n trace)]

ide :: Widget HTML a
ide = do
  nodeIdChan <- liftIO $ newBroadcastTChanIO
  nodeIdChan' <- liftIO . atomically . dupTChan $ nodeIdChan

  stepsChan <- liftIO $ newBroadcastTChanIO
  stepsChan' <- liftIO . atomically . dupTChan $ stepsChan

  traceVar <- liftIO $ newTVarIO (symexecTrace 0)

  (topWidget stepsChan
     <|> sourceWidget
     <|> (traceWidget traceVar 0 stepsChan' nodeIdChan'
          <|> stateWidget traceVar 0 nodeIdChan'))
