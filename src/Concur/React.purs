module Concur.React where

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Plus (class Alt, alt, class Plus, empty)
import Data.Array (foldl)
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

type RenderFunc v eff a = (Widget v eff a -> EventHandler eff Unit) -> v
data Widget v eff a = RenderEnd a | Widget (RenderFunc v eff a) | WidgetEff (EventHandler eff (Widget v eff a))

instance renderFunctor :: Functor (Widget v eff) where
  map f (RenderEnd a) = RenderEnd (f a)
  map f (Widget r) = Widget $ \h -> r (h <<< map f)
  map f (WidgetEff eff) = WidgetEff (map f <$> eff)

instance renderBind :: Bind (Widget v eff) where
  bind (RenderEnd a) k = k a
  bind (Widget r) k = Widget $ \h -> r (h <<< flip bind k)
  bind (WidgetEff eff) k = WidgetEff (flip bind k <$> eff)

instance renderApplicative :: Applicative (Widget v eff) where
  pure = RenderEnd

instance renderApply :: Apply (Widget v eff) where
  apply = ap

instance renderMonad :: Monad (Widget v eff)

-- TODO: Make Widget TailRecursive
-- instance renderMonadRec :: MonadRec (Widget v eff) where

instance renderAlt :: Semigroup v => Alt (Widget v eff) where
  alt (RenderEnd a) _ = RenderEnd a
  alt _ (RenderEnd a) = RenderEnd a
  alt (WidgetEff eff) g = WidgetEff (flip alt g <$> eff)
  alt f (WidgetEff eff) = WidgetEff (alt f <$> eff)
  alt (Widget f) (Widget g) = Widget $ \h ->
      let hf (RenderEnd a) = h (RenderEnd a)
          hf ff = h (alt ff (Widget g))
          hg (RenderEnd b) = h (RenderEnd b)
          hg gg = h (alt (Widget f) gg)
      in f hf <> g hg

instance renderPlus :: Monoid v => Plus (Widget v eff) where
  empty = display mempty

instance renderAlternative :: Monoid v => Alternative (Widget v eff)

mapView :: forall a v eff. (v -> v) -> Widget v eff a -> Widget v eff a
mapView f (Widget r) = Widget $ f <<< r
mapView f (WidgetEff eff) = WidgetEff (mapView f <$> eff)
mapView _ r = r

type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

never :: forall a v eff. Monoid v => Widget v eff a
never = empty

orr :: forall a v eff. Monoid v => Array (Widget v eff a) -> Widget v eff a
orr = foldl (<|>) never

el :: forall a eff. NodeName -> Widget HTML eff a -> Widget HTML eff a
el n = mapView (\v -> [n v])

el' :: forall a eff. NodeName -> Array (Widget HTML eff a) -> Widget HTML eff a
el' n = el n <<< orr

display :: forall a v eff. v -> Widget v eff a
display = Widget <<< const

liftRenderEff :: forall v eff a. EventHandler eff a -> Widget v eff a
liftRenderEff eff = WidgetEff (pure <$> eff)

instance renderMonadEff :: MonadEff eff (Widget v eff) where
  liftEff :: forall v eff a. Eff eff a -> Widget v eff a
  liftEff = liftRenderEff <<< unsafeCoerceEff

type EventHandler eff a = Eff (ReactEff R.ReadWrite R.ReadOnly eff) a
type RenderHandler eff a = Eff (ReactEff R.ReadOnly () eff) a
type This props v eff a = R.ReactThis props (Widget v eff a)

componentClass :: forall props eff a. Widget HTML eff a -> R.ReactClass props
componentClass init = R.createClass (R.spec init render)
  where
    render :: This props HTML eff a -> RenderHandler eff R.ReactElement
    render this = do
      v <- renderInner this
      -- TODO: REFINE THE DIV WRAPPER. THIS IS JUST A PLACEHOLDER
      pure (D.div' v)
    renderInner :: This props HTML eff a -> RenderHandler eff HTML
    renderInner this = do
      ren <- R.readState this
      renderComponentInner this ren
    renderComponentInner :: This props HTML eff a -> Widget HTML eff a -> RenderHandler eff HTML
    renderComponentInner this (RenderEnd a) = pure []
    renderComponentInner this (Widget r) = do
      let
        handler w = case w of
          -- TODO: POTENTIALLY INFINITE RECURSION WILL BLOW THE STACK.
          -- Something like this will trigger it - `forever (liftEff someAction)`
          (WidgetEff s) -> s >>= handler
          w' -> void (R.writeState this w')
      pure (r handler)
    -- TODO: AN EFFECTFUL WIDGET SHOULD NEVER BE PASSED TO RENDER. BREAK OUT WidgetNoEff.
    renderComponentInner this (WidgetEff eff) = pure []

renderComponent :: forall a eff. Widget HTML eff a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
