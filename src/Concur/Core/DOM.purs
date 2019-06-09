module Concur.Core.DOM where

import Concur.Core (mkLeafWidget, wrapViewEvent)
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Props (Props, mkProp)
import Concur.Core.Types (Widget)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Function (($), (<<<))
import Data.Functor (map)

-- | Wrap a single widget with a node that can have eventHandlers attached
el
  :: forall m a p v
  .  ShiftMap (Widget (Array v)) m
  => (Array p -> Array v -> Array v)
  -> Array (Props p a)
  -> m a
  -> m a
el e props = shiftMap (\f w -> wrapViewEvent (\h v -> (e (map (mkProp h <<< map f) props) v)) w)

-- | Promote a leaf node to a widget
elLeaf
  :: forall p v m a
  .  LiftWidget (Array v) m
  => (Array p -> Array v)
  -> Array (Props p a)
  -> m a
elLeaf e props = liftWidget $ mkLeafWidget \h -> e (map (mkProp h) props)

-- | Wrap some widgets with a node that can have eventHandlers attached
el'
  :: forall m a p v
  .  ShiftMap (Widget (Array v)) m
  => MultiAlternative m
  => (Array p -> Array v -> Array v)
  -> Array (Props p a)
  -> Array (m a)
  -> m a
el' e props = el e props <<< orr
