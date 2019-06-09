module Concur.React where

import Prelude

import Concur.Core.Discharge (discharge, dischargePartialEffect)
import Concur.Core.Types (Widget)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Console (log)
import React as R

type HTML
  = Array R.ReactElement

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
