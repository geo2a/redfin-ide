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

symexecTrace :: Trace Context
symexecTrace = Example.symexecTrace 100

topWidget :: Widget HTML a
topWidget =
  div [classList [ ("pane", True)
                 , ("toppane", True)
                 ]
      ]
      [text "aaa"]

sourceWidget :: Widget HTML a
sourceWidget =
  div [classList [ ("pane", True)
                 , ("leftpane", True)
                 ]
      ]
      [ol [classList [("sourceCode", True)]] (map (li [] . (:[]) . text) src)]
  where src :: [Text.Text]
        src = map (Text.pack . show . snd) $ assemble Example.addLowLevel

traceWidget :: TChan NodeId -> Widget HTML a
traceWidget nodeIdVar = do
  n <- div [classList [ ("pane", True)
                      , ("middlepane", True)
                      ]
           ]
       [pre [] [htmlTrace symexecTrace]]
  liftIO . atomically $ writeTChan nodeIdVar n
  traceWidget nodeIdVar


stateWidget :: NodeId -> TChan NodeId -> Widget HTML a
stateWidget n nodeIdChan = do
  contents <- Old <$> widget
          <|> New <$> (liftIO . atomically $ readTChan nodeIdChan)
  case contents of
    Old _  -> stateWidget n nodeIdChan
    New n' -> stateWidget n' nodeIdChan
  where widget =
          div [classList [ ("pane", True)
                         , ("rightpane", True)
                         ]
              ]
          [displayContext (lookup n symexecTrace)]

ide :: Widget HTML a
ide = do
  nodeIdChan <- liftIO $ newBroadcastTChanIO
  (topWidget
     <|> sourceWidget
     <|> ((liftIO . atomically . dupTChan $ nodeIdChan) >>=
            \c -> traceWidget c <|> stateWidget 0 c))
