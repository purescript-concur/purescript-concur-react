module Concur.React where

import Prelude

import Concur.Core.Types (Result(..), Widget, runWidget)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
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
      View v -> do
        mt <- Ref.read thisRef
        case mt of
          Just this -> void $ R.writeState this (mkComponentState v)
          Nothing -> renderer $ element onMount v thisRef
      _ -> log "Application exited"

render ::
  ComponentState ->
  R.ReactElement
render st = R.toElement st.view

toReactClass :: forall props a. String -> HTML -> (Record props -> Widget HTML a) -> R.ReactClass (Record props)
toReactClass componentName initialView widgetBuilder = R.component componentName \this -> do
  props <- R.getProps this
  let widget = widgetBuilder props
  _ <- runWidget widget \res -> do
    case res of
      View v -> do
        R.writeState this (mkComponentState v)
      _ -> log "Application exited"
  pure { state: mkComponentState initialView
       , render: render <$> R.getState this
       }

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
