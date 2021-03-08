module Redfin.IDE.Widget where

import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica            hiding (id)
import qualified Concur.Replica.DOM.Events as P
import           Control.Applicative       (Alternative, empty, (<|>))
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq, (<|), (|>))
import qualified Data.Sequence             as Seq
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text, unpack)
import           Prelude                   hiding (span)

-- | Add a hover tooltip to a widget
tooltipped :: Text -> Widget HTML a -> Widget HTML a
tooltipped tip w = span [classList [("tooltip", True)]]
  [w, span [classList [("tooltiptext", True)]] [text tip]]

-- | Extract an event's value
getValue :: BaseEvent -> String
getValue = unpack . targetValue . target
