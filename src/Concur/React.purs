module Concur.React where

import Prelude

import Concur.Core (Widget(Widget), WidgetStep(WidgetStep))
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (resume)
import Data.Either (Either(..))
import React as R
import React.DOM as D
import React.DOM.Props as P

type ReactEff state refs eff =
  ( state :: R.ReactState state
  , props :: R.ReactProps
  , refs  :: R.ReactRefs refs
  | eff
  )

type HTML = Array R.ReactElement
type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

-- These are needed because Purescript row effects are not smart enough to figure out that -
-- RenderHandler is strictly a subset of EventHandler
renderToEventHandler :: forall eff a. RenderHandler eff a -> EventHandler eff a
renderToEventHandler = unsafeCoerceEff
renderToEventHandlerAff :: forall eff a. RenderHandlerAff eff a -> EventHandlerAff eff a
renderToEventHandlerAff = unsafeCoerceAff
-- Eff is strictly a subset of RenderHandler
effToRenderHandler :: forall eff a. Eff eff a -> RenderHandler eff a
effToRenderHandler = unsafeCoerceEff
affToRenderHandlerAff :: forall eff a. Aff eff a -> RenderHandlerAff eff a
affToRenderHandlerAff = unsafeCoerceAff
-- Eff is strictly a subset of EventHandler
effToEventHandler :: forall eff a. Eff eff a -> EventHandler eff a
effToEventHandler = unsafeCoerceEff
affToEventHandlerAff :: forall eff a. Aff eff a -> EventHandlerAff eff a
affToEventHandlerAff = unsafeCoerceAff

type EventHandler eff a = Eff (ReactEff R.ReadWrite R.ReadOnly eff) a
type RenderHandler eff a = Eff (ReactEff R.ReadOnly () eff) a
type EventHandlerAff eff a = Aff (ReactEff R.ReadWrite R.ReadOnly eff) a
type RenderHandlerAff eff a = Aff (ReactEff R.ReadOnly () eff) a
type This props v eff a = R.ReactThis props (Widget v eff a)

componentClass :: forall props eff a. Widget HTML eff a -> R.ReactClass props
componentClass init = R.createClass (R.spec init render)
  where
    render :: This props HTML eff a -> RenderHandler eff R.ReactElement
    render this = do
      Widget w <- R.readState this
      case resume w of
        Right _ -> pure (D.div' [])
        Left (WidgetStep mws) -> do
          ws <- effToRenderHandler mws
          -- TODO: The unsafeCoerceAff is needed because we are allowed to write to state in the forked thread, but the type system still wants a RenderHandlerAff
          launchAff_ (affToRenderHandlerAff ws.cont >>= (unsafeCoerceAff <<< liftEff <<< R.writeState this <<< Widget))
          -- TODO: Refine the div wrapper. This is just a placeholder.
          pure (D.div' ws.view)

renderComponent :: forall a eff. Widget HTML eff a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
