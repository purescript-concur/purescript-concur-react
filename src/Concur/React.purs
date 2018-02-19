module Concur.React where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Plus (class Alt, alt, class Plus, empty)
import Control.Alternative (class Alternative, (<|>))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Data.Array (foldl)
import Data.Monoid (class Monoid, mempty)

type ReactEff state refs =
  ( state :: R.ReactState state
  , props :: R.ReactProps
  , refs  :: R.ReactRefs refs
  )

type HTML = Array R.ReactElement

type RenderFunc v a = (Widget v a -> EventHandler Unit) -> v
data Widget v a = RenderEnd a | Widget (RenderFunc v a)

instance renderFunctor :: Functor (Widget v) where
  map f (RenderEnd a) = RenderEnd (f a)
  map f (Widget r) = Widget $ \h -> r (h <<< map f)

instance renderBind :: Bind (Widget v) where
  bind (RenderEnd a) k = k a
  bind (Widget r) k = Widget $ \h -> r (h <<< flip bind k)

instance renderApplicative :: Applicative (Widget v) where
  pure = RenderEnd

instance renderApply :: Apply (Widget v) where
  apply = ap

instance renderMonad :: Monad (Widget v)

-- TODO: Make Widget TailRecursive
-- instance renderMonadRec :: MonadRec (Widget v) where

instance renderAlt :: Semigroup v => Alt (Widget v) where
  alt (RenderEnd a) _ = RenderEnd a
  alt _ (RenderEnd a) = RenderEnd a
  alt (Widget f) (Widget g) = Widget $ \h ->
    let hf (RenderEnd a) = h (RenderEnd a)
        hf ff = h (alt ff (Widget g))
        hg (RenderEnd b) = h (RenderEnd b)
        hg gg = h (alt (Widget f) gg)
    in f hf <> g hg

instance renderPlus :: Monoid v => Plus (Widget v) where
  empty = display mempty

instance renderAlternative :: Monoid v => Alternative (Widget v)

mapView :: forall a v. (v -> v) -> Widget v a -> Widget v a
mapView f (Widget r) = Widget $ f <<< r
mapView _ r = r

type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

never :: forall a v. Monoid v => Widget v a
never = empty

orr :: forall a v. Monoid v => Array (Widget v a) -> Widget v a
orr = foldl (<|>) never

el :: forall a. NodeName -> Widget HTML a -> Widget HTML a
el n = mapView (\v -> [n v])

el' :: forall a. NodeName -> Array (Widget HTML a) -> Widget HTML a
el' n = el n <<< orr

display :: forall a v. v -> Widget v a
display = Widget <<< const

type EventHandler a = Eff (ReactEff R.ReadWrite R.ReadOnly) a
type RenderHandler a = Eff (ReactEff R.ReadOnly ()) a
type This props v a = R.ReactThis props (Widget v a)

componentClass :: forall props a. Widget HTML a -> R.ReactClass props
componentClass init = R.createClass (R.spec init render)
  where
    render :: This props HTML a -> RenderHandler R.ReactElement
    render this = do
      v <- renderInner this
      -- TODO: REFINE THE DIV WRAPPER. THIS IS JUST A PLACEHOLDER
      pure (D.div' v)
    renderInner :: This props HTML a -> RenderHandler HTML
    renderInner this = do
      ren <- R.readState this
      case ren of
        RenderEnd a -> pure []
        Widget renderView -> do
          let handler r' = void (R.writeState this r')
          pure (renderView handler)

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
