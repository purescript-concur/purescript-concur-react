module Concur.React where

import Prelude

import Concur.Core (mkLeafWidget, wrapViewEvent)
import Concur.Core.Discharge (discharge, dischargePartialEffect)
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Props (mkProp)
import Concur.Core.Types (Widget)
import Concur.React.Props (ReactProps)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import React as R
import React.DOM.Props as P

type HTML
  = Array R.ReactElement

type NodeName
  = Array R.ReactElement -> R.ReactElement

type NodeTag
  = Array P.Props -> Array R.ReactElement -> R.ReactElement

type LeafTag
  = Array P.Props -> R.ReactElement

-- | Wrap a widget with a node that can have eventHandlers attached
el ::
  forall m a.
  ShiftMap (Widget HTML) m =>
  NodeTag ->
  Array (ReactProps a) ->
  m a ->
  m a
el e props = shiftMap (\f w -> wrapViewEvent (\h v ->
  [e (map (mkProp h <<< map f) props) v]) w)

-- | Promote a leaf node to a widget
elLeaf ::
  forall m a.
  LiftWidget HTML m =>
  LeafTag ->
  Array (ReactProps a) ->
  m a
elLeaf e props = liftWidget $ mkLeafWidget \h ->
  [e (map (mkProp h) props)]

-- | Wrap some widgets with a node that can have eventHandlers attached
el' ::
  forall m a.
  ShiftMap (Widget HTML) m =>
  MultiAlternative m =>
  NodeTag ->
  Array (ReactProps a) ->
  Array (m a) ->
  m a
el' e props = el e props <<< orr

-- React apparently requires wrapping state inside an object
type ComponentState
  = {view :: HTML}

mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

componentClass :: forall a. Widget HTML a -> R.ReactClass {}
componentClass winit = R.component "Concur" component
  where
  component this = do
    Tuple winit' v <- dischargePartialEffect winit
    pure { state: mkComponentState v
         , render: render <$> R.getState this
         , componentDidMount: handler this (Right winit')
         }
  handler this (Right r) = do
    v <- discharge (handler this) r
    void $ R.writeState this (mkComponentState v)
  handler _ (Left err) = do
    log ("FAILED! " <> show err)
    pure unit
  render st = R.toElement st.view

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createLeafElement (componentClass init) {}
