module Test.Wire where

import Concur.Core.Patterns (Wire, local, mapWire, with, send)
import Concur.Core.Types (Widget, silence)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Bind (discard)
import Data.Function (($))
import Data.Functor (map, void)
import Data.Lens as L
import Data.Ord ((>=))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Void (absurd)
import Prelude ((+))

-- counter :: forall b. Wire (Widget HTML) Int -> Widget HTML b
-- counter wire = map absurd do
--   with wire \x -> do
--     if x >= 10
--      then D.text "10"
--      else do
--        void $ D.button [ P.onClick ] [ D.text $ show x ]
--        send wire $ x + 1

-- wireWidget :: forall a. Widget HTML a
-- wireWidget = local (Tuple 0 0) \wire -> D.div []
--   [ D.div' [D.text "This counter is independent of the other two "]
--   , counter (mapWire L.first wire)
--   , D.div' [D.text "These two counters have the same state"]
--   , counter (mapWire L.second wire)
--   , counter (mapWire L.second wire)
--   ]
