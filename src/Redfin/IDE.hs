{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Redfin.IDE
  ( ide
  ) where

import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica                hiding (id)
import qualified Concur.Replica.DOM.Events     as P
import           Control.Applicative           (empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader
import           Control.ShiftMap
import qualified Data.Aeson                    as A
import           Data.Functor                  (void)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Text
import qualified Data.Text.Read                as Text
import           Prelude                       hiding (div, lookup)
import           Replica.VDOM.Render           as Render

import           ISA.Assembly                  (Script, assemble)
import           ISA.Backend.Symbolic.List.Run (runModel)
import           ISA.Types.Instruction.Decode
import           ISA.Types.Symbolic.Context
import           ISA.Types.Symbolic.Trace      hiding (htmlTrace)

import           Redfin.IDE.State
import           Redfin.IDE.Trace              (htmlTrace)
import           Redfin.IDE.Types

import qualified ISA.Example.Add               as Example
import qualified ISA.Example.Sum               as ExampleSum

topWidget :: Steps -> App a
topWidget steps ide = do
  steps' <-
    div [classList [ ("pane", True)
                   , ("toppane", True)
                   ]
        ]
        [stepsWidget steps]
  liftIO . atomically $ putTMVar (_steps ide) steps'
  liftIO . atomically $ writeTVar (_trace ide) ((_runSymExec ide) steps')
  topWidget steps' ide

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
        src = map (Text.pack . show . snd) $ assemble (_source state)

traceWidget :: Steps
            -> App a
traceWidget steps ide =
  widget <|> (liftIO . atomically $ takeTMVar (_steps ide)) >>=
    \steps' -> traceWidget steps' ide
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO . atomically $ readTVar (_trace ide)
      div [classList [ ("pane", True)
                     , ("middlepane", True)
                     ]
          ]
          [pre [] [htmlTrace trace ide]]

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

data Example = ExampleAdd
             | ExampleSum
             | ExampleGCD
             | ExampleMotor
  deriving (Show, Eq)

mkIDE :: Example -> IO IDEState
mkIDE ex = do
  nodeIdVar <- liftIO $ newTMVarIO 0
  stepsVar  <- liftIO $ newTMVarIO 0
  case ex of
    ExampleAdd   -> do
      traceVar  <- liftIO $ newTVarIO (Example.symexecTrace 0)
      pure (IDEState traceVar stepsVar nodeIdVar
             Example.addLowLevel
             Example.symexecTrace)
    ExampleSum   -> do
      traceVar  <- liftIO $ newTVarIO (runModel 0 ExampleSum.initContext)
      pure (IDEState traceVar stepsVar nodeIdVar
             ExampleSum.sumArrayLowLevel
             (\s -> runModel s ExampleSum.initContext))
    ExampleGCD   -> error "Not implemented"
    ExampleMotor -> error "Not implemented"

ide :: Widget HTML a
ide = do
  initialState <- liftIO $ mkIDE ExampleSum
  foldl (<|>) empty . map ($ initialState) $
    [ topWidget 0
    , sourceWidget
    , traceWidget 0
    , stateWidget 0
    ]
