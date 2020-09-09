{-# LANGUAGE ScopedTypeVariables #-}

module Redfin.IDE
  ( ide
  ) where

import           Concur.Core
import           Concur.Replica               hiding (id)
import qualified Concur.Replica.DOM.Events    as P
import           Control.Applicative          (empty, (<|>))
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

data IDEState =
  IDEState { _trace      :: TVar (Trace Context)
           , _steps      :: TMVar Steps
           , _activeNode :: TMVar NodeId
           }

symexecTrace :: Steps -> Trace Context
symexecTrace steps = Example.symexecTrace steps

topWidget :: Steps -> TMVar Steps -> Widget HTML a
topWidget steps stepsVar = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [controlWidget steps]
  liftIO . atomically $ putTMVar stepsVar steps'
  topWidget steps' stepsVar

-- examplesWidget :: Widget HTML a
-- examplesWidget =
--   ul [] [ li [] [text "Add"]
--         , li [] [text "Sum"]
--         , li [] [text "GCD"]
--         , li [] [text "Motor control"]
--         ]

controlWidget :: Steps -> Widget HTML Steps
controlWidget s =
  read . Text.unpack . targetValue . target <$>
    div []
      [ text ("Symbolic execution steps: " <> Text.pack (show s))
      , input [placeholder "Change", onChange, autofocus True]
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
            -> TMVar NodeId
            -> TMVar Steps -> Steps
            -> Widget HTML a
traceWidget traceVar nodeIdVar stepsVar steps =
  widget <|> (liftIO . atomically $ takeTMVar stepsVar) >>=
  traceWidget traceVar nodeIdVar stepsVar
  where
    widget :: Widget HTML Steps
    widget = do
          let trace = symexecTrace steps
          n <- div [classList [ ("pane", True)
                              , ("middlepane", True)
                              ]
                   ]
               [pre [] [htmlTrace trace]]
          liftIO . atomically $ writeTVar traceVar trace *>
                                putTMVar nodeIdVar n
          pure steps

-- | TODO: Maybe invalidate the state widget after changing the amount of steps,
--   since nodes can have different ids in different partial traces
stateWidget :: TVar (Trace Context) -> TMVar NodeId -> NodeId -> Widget HTML a
stateWidget traceVar nodeIdVar n =
  widget <|> (liftIO . atomically $ takeTMVar nodeIdVar) >>=
  stateWidget traceVar nodeIdVar
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO . atomically $ readTVar traceVar
      div [classList [ ("pane", True)
                     , ("rightpane", True)
                     ]
          ]
          [ h3 [] [ text $ "State in node " <> Text.pack (show n) <> ": "]
          , displayContext (lookup n trace)
          ]

ide :: Widget HTML a
ide = do
  nodeIdVar <- liftIO $ newTMVarIO 0
  stepsVar  <- liftIO $ newTMVarIO 0
  traceVar  <- liftIO $ newTVarIO (symexecTrace 0)

  foldl (<|>) empty
    [ topWidget 0 stepsVar
    , sourceWidget
    , (traceWidget traceVar nodeIdVar stepsVar 0)
    , stateWidget traceVar nodeIdVar 0
    ]
