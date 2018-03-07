module Concur.React where

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Plus (class Alt, alt, class Plus, empty)
import Data.Array (foldl)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
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

data Free f a = End a | Free (f (Free f a))

type WidgetHandler v eff a = Widget v eff a -> EventHandler eff Unit
newtype WidgetStep v eff = WidgetStep
  { view :: v
  , cont :: Maybe (EventHandlerAff eff Unit)
  }
data Widget v eff a = RenderEnd a | Widget (WidgetHandler v eff a -> WidgetStep v eff)

instance widgetStepSemigroup :: Semigroup v => Semigroup (WidgetStep v eff) where
  append (WidgetStep ws1) (WidgetStep ws2) = WidgetStep { view : v', cont : r' }
    where
      v' = ws1.view <> ws2.view
      r' = ws1.cont <> ws2.cont

viewStep :: forall v eff. v -> WidgetStep v eff
viewStep v = WidgetStep { view: v, cont: Nothing }

instance widgetStepMonoid :: Monoid v => Monoid (WidgetStep v eff) where
  mempty = viewStep mempty

instance widgetFunctor :: Functor (Widget v eff) where
  map f (RenderEnd a) = RenderEnd (f a)
  map f (Widget r) = Widget (\h -> r (h <<< map f))

instance widgetBind :: Bind (Widget v eff) where
  bind (RenderEnd a) k = k a
  bind (Widget r) k = Widget (\h -> r (\w -> h (bind w k)))

instance widgetApplicative :: Applicative (Widget v eff) where
  pure = RenderEnd

instance widgetApply :: Apply (Widget v eff) where
  apply = ap

instance widgetMonad :: Monad (Widget v eff)

-- NOTE: Widgets are already tail recursion safe, thanks to setState effectively being an effectful trampoline.
-- However, we still need a MonadRec instance to allow monad transformers like StateT on top.
-- See this discussion on github - https://github.com/ajnsit/purescript-concur/issues/1
instance widgetMonadRec :: MonadRec (Widget v eff) where
  tailRecM :: forall v eff a b. (a -> Widget v eff (Step a b)) -> a -> Widget v eff b
  tailRecM f a = go =<< f a
    where
      go (Loop a') = tailRecM f a'
      go (Done b) = pure b

instance widgetAlt :: Semigroup v => Alt (Widget v eff) where
  alt (RenderEnd a) _ = RenderEnd a
  alt _ (RenderEnd a) = RenderEnd a
  alt (Widget f) (Widget g) = Widget $ \h ->
      let hf (RenderEnd a) = h (RenderEnd a)
          hf ff = h (alt ff (Widget g))
          hg (RenderEnd b) = h (RenderEnd b)
          hg gg = h (alt (Widget f) gg)
      in f hf <> g hg

instance widgetPlus :: Monoid v => Plus (Widget v eff) where
  empty = display mempty

instance widgetAlternative :: Monoid v => Alternative (Widget v eff)

mapView :: forall a v eff. (v -> v) -> Widget v eff a -> Widget v eff a
mapView f (Widget r) = Widget (mapViewStep f <<< r)
mapView _ r = r

mapViewStep :: forall v1 v2 eff. (v1 -> v2) -> WidgetStep v1 eff -> WidgetStep v2 eff
mapViewStep f (WidgetStep ws) = WidgetStep (ws { view = f ws.view })

forViewStep :: forall v1 v2 eff.  WidgetStep v1 eff -> (v1 -> v2) -> WidgetStep v2 eff
forViewStep (WidgetStep ws) f = WidgetStep (ws { view = f ws.view })

type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

never :: forall a v eff. Monoid v => Widget v eff a
never = empty

-- Pause for a negligible amount of time. Forces continuations to pass through the trampoline.
-- Avoids stack overflows in (ridiculous) cases where a widget calls itself without any intervening widgets or effects.
-- E.g. -
--   BAD  `counter n = if n < 10000 then counter (n+1) else pure n`
--   GOOD `counter n = if n < 10000 then (pulse >>= const (counter (n+1))) else pure n`
pulse :: forall v eff. Monoid v => Widget v eff Unit
pulse = liftRenderEff (pure unit)

orr :: forall a v eff. Monoid v => Array (Widget v eff a) -> Widget v eff a
orr = foldl (<|>) never

el :: forall a eff. NodeName -> Widget HTML eff a -> Widget HTML eff a
el n = mapView (\v -> [n v])

el' :: forall a eff. NodeName -> Array (Widget HTML eff a) -> Widget HTML eff a
el' n = el n <<< orr

display :: forall a v eff. v -> Widget v eff a
display v = Widget (\h -> WidgetStep { view: v, cont: Nothing })

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

liftRenderAff :: forall v eff a. Monoid v => RenderHandlerAff eff a -> Widget v eff a
liftRenderAff aff = Widget (\h -> WidgetStep {view: mempty, cont: Just (aff' h)})
  where
    aff' h = do
      a <- renderToEventHandlerAff aff
      liftEff (h (RenderEnd a))

instance widgetMonadAff :: Monoid v => MonadAff eff (Widget v eff) where
  liftAff :: forall v eff a. Monoid v => Aff eff a -> Widget v eff a
  liftAff = liftRenderAff <<< affToRenderHandlerAff

liftRenderEff :: forall v eff a. Monoid v => RenderHandler eff a -> Widget v eff a
liftRenderEff eff = Widget (\h -> WidgetStep {view: mempty, cont: Just (liftEff (eff' h))})
  where
    eff' h = do
      a <- renderToEventHandler eff
      h (RenderEnd a)

instance widgetMonadEff :: Monoid v => MonadEff eff (Widget v eff) where
  liftEff :: forall v eff a. Monoid v => Eff eff a -> Widget v eff a
  liftEff = liftRenderEff <<< effToRenderHandler

type EventHandler eff a = Eff (ReactEff R.ReadWrite R.ReadOnly eff) a
type RenderHandler eff a = Eff (ReactEff R.ReadOnly () eff) a
type EventHandlerAff eff a = Aff (ReactEff R.ReadWrite R.ReadOnly eff) a
type RenderHandlerAff eff a = Aff (ReactEff R.ReadOnly () eff) a
type This props v eff a = R.ReactThis props (Widget v eff a)

componentClass :: forall props eff a. Widget HTML eff a -> R.ReactClass props
componentClass init = R.createClass (R.spec init render)
  where
    mkHandler this = go
      where
        go w@(RenderEnd _) = void (R.writeState this w)
        go (Widget r) = do
          case r go of
            WidgetStep ws -> do
              -- Change the UI immediately
              void (R.writeState this (Widget (const (viewStep ws.view))))
              -- Then run any effects
              case ws.cont of
                Nothing -> pure unit
                Just aff -> do
                  -- BUG: We want this to be asynchronous
                  -- But currently it seems to be synchronous
                  void (runAff (\e -> pure unit) aff) -- launchAff_ aff
    render :: This props HTML eff a -> RenderHandler eff R.ReactElement
    render this = do
      v <- renderInner this
      -- TODO: Refine the div wrapper. This is just a placeholder.
      pure (D.div' v)
    renderInner :: This props HTML eff a -> RenderHandler eff HTML
    renderInner this = do
      w <- R.readState this
      case w of
        -- NOTE: We ignore any return values while rendering
        RenderEnd a -> pure mempty
        Widget r -> case r (mkHandler this) of
          -- NOTE: We ignore any effects directly passed to render
          WidgetStep ws -> pure (ws.view)

renderComponent :: forall a eff. Widget HTML eff a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
