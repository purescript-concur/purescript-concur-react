
module Concur.React where

import Prelude

import Concur.Core.Types (Widget, runWidget)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Console (log)
import React as R

type HTML
  = Array R.ReactElement

-- React apparently requires wrapping state inside an object
type ComponentState
  = {view :: HTML}

mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

componentClassWithMount ::
  forall a.
  Effect Unit ->
  Widget HTML a ->
  (R.ReactElement -> Effect Unit) ->
  Effect Unit
componentClassWithMount onMount w renderer = do
  thisRef <- Ref.new Nothing
  void $ runWidget w \res -> do
    case res of
      Left v -> do
        mt <- Ref.read thisRef
        case mt of
          Just this -> void $ R.writeState this (mkComponentState v)
          Nothing -> renderer $ element onMount v thisRef
      Right _ -> log "Application exited"

render ::
  ComponentState ->
  R.ReactElement
render st = R.toElement st.view

component ::
  Effect Unit ->
  HTML ->
  Ref (Maybe (R.ReactThis {} ComponentState)) ->
  R.ReactThis {} ComponentState ->
  Effect { componentDidMount :: Effect Unit
  , render :: Effect R.ReactElement
  , state :: ComponentState  }
component onMount vinit ref this = do
  pure { state: mkComponentState vinit
       , render: render <$> R.getState this
       , componentDidMount: onMount *> handler this vinit ref
       }
  where
    handler t v r = do
      Ref.write (Just t) r
      void $ R.writeState t (mkComponentState v)

element ::
  Effect Unit ->
  HTML ->
  Ref (Maybe (R.ReactThis {} ComponentState)) ->
  R.ReactElement
element onMount v ref = R.createLeafElement (R.component "Concur" (component onMount v ref)) {}

renderComponent ::
  forall a.
  Widget HTML a ->
  (R.ReactElement -> Effect Unit) ->
  Effect Unit
renderComponent winit renderer = componentClassWithMount mempty winit renderer
