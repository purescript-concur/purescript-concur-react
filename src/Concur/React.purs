module Concur.React where

import Prelude

import Concur.Core.Discharge (discharge)
import Concur.Core.Types (Widget)
import Control.Monad.State (StateT, runStateT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import React as R

type HTML
  = Array R.ReactElement

-- React apparently requires wrapping state inside an object
type ComponentState s
  = { state :: s, view :: HTML}

mkComponentState :: forall s. s -> HTML -> ComponentState s
mkComponentState s v = { state: s, view: v }

componentClassWithMount :: forall a. Effect Unit -> Widget HTML a -> R.ReactClass {}
componentClassWithMount onMount winit = R.component "Concur" component
  where
    component this = do
      -- Tuple winit' v <- dischargePartialEffect winit
      pure { state: {view: mempty}
           , render: render <$> R.getState this
           , componentDidMount: onMount *> handler this (Right winit)
           }
    handler this (Right r) = do
      mv <- discharge (handler this) r
      case mv of
        Nothing -> pure unit
        Just v -> void $ R.setState this {view:v}
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    render st = R.toElement st.view

componentClass :: forall a. Widget HTML a -> R.ReactClass {}
componentClass = componentClassWithMount mempty

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createLeafElement (componentClass init) {}

statefulComponentClassWithMount :: forall a s. Effect Unit -> StateT s (Widget HTML) a -> s -> R.ReactClass {}
statefulComponentClassWithMount onMount swinit sinit = R.component "Concur" component
  where
    component this = do
      let winit = runStateT swinit sinit
      -- Tuple winit' v <- dischargePartialEffect winit
      pure { state: {state:sinit, view:mempty}
           , render: render <$> R.getState this
           , componentDidMount: onMount *> handler this (Right winit)
           }
    handler this (Right r) = do
      mv <- discharge (handler this) r
      case mv of
        Nothing -> pure unit
        Just v -> void $ R.setState this {view:v}
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    render st = R.toElement st.view
