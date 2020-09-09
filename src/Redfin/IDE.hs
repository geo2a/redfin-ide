{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Redfin.IDE
  ( ide
  ) where

import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica               hiding (id)
import qualified Concur.Replica.DOM.Events    as P
import           Control.Applicative          (empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader
import           Control.ShiftMap
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

type App a = IDEState -> Widget HTML a

symexecTrace :: Steps -> Trace Context
symexecTrace steps = Example.symexecTrace steps

topWidget :: Steps -> App a
topWidget steps state = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [stepsWidget steps]
  liftIO . atomically $ putTMVar (_steps state) steps'
  topWidget steps' state

-- -- -- examplesWidget :: Widget HTML a
-- -- -- examplesWidget =
-- -- --   ul [] [ li [] [text "Add"]
-- -- --         , li [] [text "Sum"]
-- -- --         , li [] [text "GCD"]
-- -- --         , li [] [text "Motor control"]
-- -- --         ]

stepsWidget :: Steps -> Widget HTML Steps
stepsWidget s =
  read . Text.unpack . targetValue . target <$>
    div []
      [ text ("Symbolic execution steps: " <> Text.pack (show s))
      , input [placeholder "Change", onChange, autofocus True]
      ]

sourceWidget :: App a
sourceWidget state =
  div [classList [ ("pane", True)
                 , ("leftpane", True)
                 ]
      ]
      [ol [classList [("sourceCode", True)]] (map (li [] . (:[]) . text) src)]
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) $ assemble Example.addLowLevel

traceWidget :: Steps
            -> App a
traceWidget steps state =
  widget <|> (liftIO . atomically $ takeTMVar (_steps state)) >>=
  \steps' -> traceWidget steps' state
  where
    widget :: Widget HTML Steps
    widget = do
          let trace = symexecTrace steps
          n <- div [classList [ ("pane", True)
                              , ("middlepane", True)
                              ]
                   ]
               [pre [] [htmlTrace trace]]
          liftIO . atomically $ writeTVar (_trace state) trace *>
                                putTMVar (_activeNode state) n
          pure steps

-- | TODO: Maybe invalidate the state widget after changing the amount of steps,
--   since nodes can have different ids in different partial traces
stateWidget :: NodeId -> App a
stateWidget n state =
  widget <|> (liftIO . atomically $ takeTMVar (_activeNode state)) >>=
  \n -> stateWidget n state
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO . atomically $ readTVar (_trace state)
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

  let initialState = IDEState traceVar stepsVar nodeIdVar

  foldl (<|>) empty . map ($ initialState) $
    [ topWidget 0
    , sourceWidget
    , traceWidget 0
    , stateWidget 0
    ]
