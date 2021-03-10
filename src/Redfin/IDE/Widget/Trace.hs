{-# LANGUAGE PatternSynonyms #-}

module Redfin.IDE.Widget.Trace
  ( traceWidget
  ) where

import           Colog                     (HasLog (..), LogAction, Message,
                                            pattern D, pattern E, pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica            hiding (id)
import qualified Concur.Replica.DOM.Events as P
import           Control.Applicative       (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative  as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                as A
import           Data.Either               (rights)
import           Data.Functor              (void)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy.Builder    as Text
import qualified Data.Text.Read            as Text
import           Prelude                   hiding (div, log, lookup, span)
import           Replica.VDOM.Render       as Render
import           Text.Read                 (readEither)

import           ISA.Types

import           Redfin.IDE.Trace          (htmlTrace)
import           Redfin.IDE.Types
import           Redfin.IDE.Widget

import           ISA.Assembly              (Script, assemble)

-- | Display the symbolic execution trace
--   Clicking on a node will display the node's
--   state in the state widget on the very right
traceWidget :: App a
traceWidget = widget
  where
    widget :: Widget HTML a
    widget = do
      trace <- liftIO $ readTVarIO (_trace ?ide)
      div [classList [ ("pane", True)
                     , ("middlepane", True)
                     ]
          ]
          [htmlTrace trace]
      widget
