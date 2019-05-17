module Concur.Core.FRP where

import Prelude

import Concur.Core (Widget)
import Control.Alternative (empty)
import Control.Cofree (Cofree, mkCofree, tail)
import Control.Comonad (extract)
import Data.Either (Either(..), either, hush)
import Data.Maybe (Maybe(..))

----------
-- SIGNALS
----------
-- | Poor man's FRP implementation for Concur.
-- | I am experimenting with the smallest possible amount of FRP which can still be useful.
-- | A Widget can be considered to be a one-shot Event. (There is no stream of events in Concur).
-- | Signals then are never-ending widget loops that allow access to their last return value.
-- | This last produced value allows composition with other widgets even for never-ending widgets.
type Signal v a
  = Cofree (Widget v) a

-- | Construct a signal from an initial value, and a step widget
step ::
  forall v a.
  a ->
  Widget v (Signal v a) ->
  Signal v a
step = mkCofree

-- | Display a widget which returns a continuation
display :: forall v. Widget v (Signal v Unit) -> Signal v Unit
display w = step unit w

-- | Run a widget once then stop. This will reflow when a parent signal reflows
runWidgetOnce :: forall v. Monoid v => Widget v Unit -> Signal v Unit
runWidgetOnce w = display do w *> empty

-- | A constant signal
always ::
  forall v a.
  Monoid v =>
  a ->
  Signal v a
always = pure

-- | Update signal to a new value
update ::
  forall v a.
  Signal v a ->
  Widget v (Signal v a)
update = tail

-- | Construct a signal by polling a signal with a nested widget for values
poll ::
  forall v a.
  Signal v (Widget v a) ->
  Widget v (Signal v a)
poll b = step <$> extract b <*> (map poll (update b))

-- | Create a signal which repeatedly invokes a widget for values.
-- | E.g. `signal False checkbox` will return a signal which reflects the current value of the checkbox.
hold ::
  forall v a.
  Monoid v =>
  a ->
  Widget v a ->
  Signal v a
hold a w = step a do
  a' <- w
  pure (hold a' w)

-- | Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
loopW ::
  forall v a.
  a ->
  (a -> Widget v a) ->
  Signal v a
loopW a f = step a (go <$> f a)
  where
  go x = loopW x f

-- | Loop a signal so that the return value is passed to the beginning again.
loopS ::
  forall v a.
  Monoid v =>
  a ->
  (a -> Signal v a) ->
  Signal v a
loopS a f = step (extract this) do
  s <- update this
  pure (loopS (extract s) f)
  where
  this = f a

-- | Loop a signal so that the return value is passed to the beginning again.
-- loop :: forall v a. Monoid v => (a -> Signal v (Maybe a)) -> Signal v a
-- loop f = step (extract this) do
--   s <- update this
--   pure (loopS (extract s) f)
--   where this = f Nothing
-- | Folding signals. Similar to how signals used to work in Elm.
-- | This can be used to implement simple stateful Signals.
-- | e.g. `counter = fold (\n _ -> n+1) 0 clicks`
foldp ::
  forall v a b.
  (a -> b -> a) ->
  a ->
  Signal v b ->
  Signal v a
foldp f a sb = step a' (map (foldp f a') (update sb))
  where
  a' = f a (extract sb)

-- | Consume a closed signal to make a widget
-- dyn :: forall v. (forall x. Signal v x) ~> (forall x. Widget v x)
dyn ::
  forall v a b.
  Signal v a ->
  Widget v b
dyn s = update s >>= dyn

-- | Run a signal *once* and return its value
oneShot ::
  forall v a.
  Signal v (Maybe a) ->
  Widget v a
oneShot s = case extract s of
  Nothing -> update s >>= oneShot
  Just a -> pure a

-- Very useful to embed a signal in the middle of a widget
demand ::
  forall v a.
  Signal v (Maybe a) ->
  Widget v a
demand = oneShot

demand' :: forall v a. (Maybe a -> Signal v (Maybe a)) -> Widget v a
demand' f = oneShot (f Nothing)

-- A Common pattern is demand + stateLoopS
demandLoop ::
  forall v a s.
  Monoid v =>
  s ->
  (s -> Signal v (Either s a)) ->
  Widget v a
demandLoop def w = demand (stateLoopS def w)

-- A generalisation of `loopS` where, you have an inner loop state `s` and a final result `a`
-- The loop continues as long as `Left s` is returned. And ends when `Right a` is returned.
stateLoopS ::
  forall v a s.
  Monoid v =>
  s ->
  (s -> Signal v (Either s a)) ->
  Signal v (Maybe a)
stateLoopS def w = map hush $ loopS (Left def) $ either w (pure <<< Right)
