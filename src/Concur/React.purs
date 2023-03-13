module Concur.React where

import Concur.Core (mkWidget)
import Concur.Core.Types (Result(..), Widget, runWidget)
import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind, discard)
import Data.Either (Either(..))
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Monoid (mempty)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, killFiber, runAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import React as R

type HTML
  = Array R.ReactElement

-- React requires wrapping state inside an object
type ComponentState
  = {view :: HTML}

mkComponentState :: HTML -> ComponentState
mkComponentState v = { view: v }

render ::
  ComponentState ->
  R.ReactElement
render st = R.toElement st.view

toReactClass :: forall props a. String -> HTML -> (Record props -> Widget HTML a) -> R.ReactClass (Record props)
toReactClass componentName initialView widgetBuilder = toReactClassWithMount componentName initialView mempty widgetBuilder

toReactClassWithMount :: forall props a. String -> HTML -> Effect Unit -> (Record props -> Widget HTML a) -> R.ReactClass (Record props)
toReactClassWithMount componentName initialView onMount widgetBuilder = R.component componentName \this -> do
  props <- R.getProps this
  -- TODO: Figure out why this delay is needed, else the widget doesn't render
  let widget = affAction (delay (Milliseconds 0.0)) *> widgetBuilder props
  _ <- runWidget widget \res -> do
    case res of
      View v -> do
        R.writeState this (mkComponentState v)
      _ -> log "Application exited"
  pure { state: mkComponentState initialView
       , render: render <$> R.getState this
       , componentDidMount: onMount
       }

toReactElement :: forall a. String -> HTML -> Widget HTML a -> R.ReactElement
toReactElement componentName initialView widget =
  R.createLeafElement reactClass {}
  where
  reactClass :: R.ReactClass (Record ())
  reactClass = toReactClass componentName initialView (const widget)

-- Widget that performs an async action
affAction :: forall v a. Aff a -> Widget v a
affAction aff = mkWidget \cb -> do
  fiber <- runAff (handler cb) aff
  pure do
    runAff_ mempty $ killFiber (error "cancelling aff") fiber
  where
    handler cb (Right r) = cb (Completed r)
    handler _cb (Left _) = log "error calling aff"
