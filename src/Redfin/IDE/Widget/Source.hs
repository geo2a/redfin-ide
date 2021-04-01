module Redfin.IDE.Widget.Source
  ( sourceWidget
  ) where

import           Colog                       (HasLog (..), LogAction, Message,
                                              pattern D, pattern E, pattern I)
import           Concur.Core
import           Concur.Core.Types
import           Concur.Replica
import qualified Concur.Replica.DOM.Events   as P
import           Control.Applicative         (Alternative, empty, (<|>))
import           Control.Concurrent.STM
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import qualified Control.MultiAlternative    as MultiAlternative
import           Control.ShiftMap
import qualified Data.Aeson                  as A
import           Data.Bifunctor
import           Data.Either                 (rights)
import           Data.Functor                (void)
import           Data.Int                    (Int32)
import qualified Data.IntMap                 as IntMap
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy.Builder      as Text
import qualified Data.Text.Read              as Text
import           Prelude                     hiding (div, id, log, lookup, span)
import           Replica.VDOM.Render         as Render
import           Text.Read                   (readEither)

import           ISA.Assembly                (Script, assemble)
import           ISA.Backend.Symbolic.Zipper hiding (_trace)
import           ISA.Types
import           ISA.Types.Context
import           ISA.Types.Instruction
import           ISA.Types.Key
import           ISA.Types.Symbolic          hiding (getValue)

import           Redfin.IDE.Types
import           Redfin.IDE.Widget

-- | Display the assembly source code of the program
sourceWidget :: FilePath -> App a
sourceWidget fpath = do
  log D "Displaying source code"
  void $ div [classList [ ("box", True)
                        , ("sourceWidget", True)]]
           [ h3 [] [text "Source code"]
           , ol [classList [("listing", True)]]
             (map lineView src)
           ]
  sourceWidget fpath

  where src = map (first (+ 1)) $ _source ?ide

        lineView line =
          li [ id (Text.pack (show . fst $ line))]
             [pre []
               [ span [classList [("pointer", True)]] []
               , code [classList [("haskell", True)]] [text $ Text.pack (show . snd $ line)]]
             ]
