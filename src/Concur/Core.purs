module Concur.Core where

import Prelude

import Control.Alternative (class Alternative, (<|>))
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Plus (class Alt, alt, class Plus, empty)
import Data.Array (foldl)
import Data.Functor.Contravariant (class Contravariant, cmap)

-- TODO: Use Free, or something more performant

type RenderFunc v m c a = c (Widget v m c a) -> v
data Widget v m c a = WidgetEnd a | WidgetRender (RenderFunc v m c a) | WidgetMonad (m (Widget v m c a))

instance renderFunctor :: (Functor m, Contravariant c) => Functor (Widget v m c) where
  map f (WidgetEnd a) = WidgetEnd (f a)
  map f (WidgetRender r) = WidgetRender $ \h -> r (cmap (map f) h)
  map f (WidgetMonad m) = WidgetMonad (map f <$> m)

instance renderBind :: (Monad m, Contravariant c) => Bind (Widget v m c) where
  bind (WidgetEnd a) k = k a
  bind (WidgetRender r) k = WidgetRender $ \h -> r (cmap (flip bind k) h)
  bind (WidgetMonad m) k = WidgetMonad (flip bind k <$> m)

instance renderApplicative :: (Monad m, Contravariant c) => Applicative (Widget v m c) where
  pure = WidgetEnd

instance renderApply :: (Monad m, Contravariant c) => Apply (Widget v m c) where
  apply = ap

instance renderMonad :: (Monad m, Contravariant c) => Monad (Widget v m c)

-- Currently there is no guarantee of stack safety
-- instance renderMonadRec :: (Monad m, Contravariant c, MonadRec m) => MonadRec (Widget v m c) where
--   -- tailRecM :: forall v m c a b. (a -> Widget v m c (Step a b)) -> a -> Widget v m c b
--   tailRecM f a = go =<< f a
--     where
--       go (Loop a') = tailRecM f a'
--       go (Done b) = pure b

instance renderAlt :: (Functor m, Contravariant c, Semigroup v) => Alt (Widget v m c) where
  alt (WidgetEnd a) _ = WidgetEnd a
  alt _ (WidgetEnd a) = WidgetEnd a
  alt (WidgetMonad m) g = WidgetMonad (flip alt g <$> m)
  alt f (WidgetMonad m) = WidgetMonad (alt f <$> m)
  alt (WidgetRender f) (WidgetRender g) = WidgetRender $ \h ->
      let hf (WidgetEnd a) = WidgetEnd a
          hf ff = alt ff (WidgetRender g)
          hg (WidgetEnd b) = WidgetEnd b
          hg gg = alt (WidgetRender f) gg
      in f (cmap hf h) <> g (cmap hg h)

instance renderPlus :: (Functor m, Monoid v, Contravariant c) => Plus (Widget v m c) where
  empty = display mempty

instance renderAlternative :: (Monad m, Contravariant c, Monoid v) => Alternative (Widget v m c)

mapView :: forall v m c a. Functor m => (v -> v) -> Widget v m c a -> Widget v m c a
mapView f (WidgetRender r) = WidgetRender $ f <<< r
mapView f (WidgetMonad m) = WidgetMonad (mapView f <$> m)
mapView _ r = r

never :: forall v m c a. Functor m => Monoid v => Contravariant c => Widget v m c a
never = empty

orr :: forall v m c a. Functor m => Monoid v => Contravariant c => Array (Widget v m c a) -> Widget v m c a
orr = foldl (<|>) never

display :: forall v m c a. v -> Widget v m c a
display = WidgetRender <<< const

liftMonad :: forall v m c a. Monad m => Contravariant c => m a -> Widget v m c a
liftMonad m = WidgetMonad (pure <$> m)

wrapMonad :: forall v m c a. m (Widget v m c a) -> Widget v m c a
wrapMonad m = WidgetMonad m

newtype EffectMaker v m c a = EffectMaker (m
  { handler :: c (Widget v m c a)
  , observer :: m (Widget v m c a)
  })
newtype Renderer v m = Renderer (v -> forall a. m a)

-- Runner
runWidget :: forall v m c a. Monad m => Alt m => EffectMaker v m c a -> Renderer v m -> Widget v m c a -> m a
runWidget (EffectMaker e) (Renderer r) = go
  where
  go w = case w of
    WidgetEnd a -> pure a
    WidgetMonad m -> m >>= go
    WidgetRender f -> do
      x <- e
      w' <- r (f x.handler) <|> x.observer
      go w'
