module Concur.React where

import Prelude

import Concur.Core (Widget, wrapViewEvent, mkLeafWidget)
import Concur.Core.Discharge (discharge, dischargePartialEffect)
import Concur.React.Props (Props, mkProp)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, class ShiftUp, shiftMap, shiftUp)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Unsafe.Coerce (unsafeCoerce)

type HTML = Array R.ReactElement
type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement
type LeafTag = Array P.Props -> R.ReactElement

-- BIG HACK! We use UnsafeCoerce to allow this to typecheck. This MIGHT cause RUNTIME errors! Verify!
-- | Wrap a widget with a node that can have eventHandlers attached
el :: forall m a. ShiftMap (Widget HTML) m => NodeTag -> Array (Props a) -> m a -> m a
el e props = shiftMap (wrapViewEvent \h v -> [e (map (mkProp h) (unsafeCoerce props)) v])

-- | Promote a leaf node to a widget
elLeaf :: forall m a. ShiftUp (Widget HTML) m => LeafTag -> Array (Props a) -> m a
elLeaf e props = shiftUp (mkLeafWidget \h -> [e (map (mkProp h) (unsafeCoerce props))])

-- | Wrap some widgets with a node that can have eventHandlers attached
el' :: forall m a. ShiftMap (Widget HTML) m => MultiAlternative m => NodeTag -> Array (Props a) -> Array (m a) -> m a
el' e props = el e props <<< orr

-- React apparently requires wrapping state inside an object
type ComponentState =  { view :: HTML }
mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

componentClass :: forall a. Widget HTML a -> R.ReactClass {}
componentClass winit = R.component "Concur" component
  where
    component this = do
      Tuple winit' v <- dischargePartialEffect winit
      pure
        { state: mkComponentState v
        , render: render <$> R.getState this
        , componentDidMount: handler this (Right winit')
        }
    handler this (Right r) = do
      v <- discharge (handler this) r
      void $ R.writeState this (mkComponentState v)
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    -- TODO: Refine the div wrapper. This is just a placeholder.
    render st = D.div' st.view

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createLeafElement (componentClass init) {}
  -- R.createFactory (componentClass init) {}
