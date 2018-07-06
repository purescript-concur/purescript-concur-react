module Concur.Core.FRP where

import Prelude

import Concur.Core (Widget, WidgetCombinator)
import Control.Alternative (empty, (<|>))
import Control.Comonad (class Comonad, class Extend, extract)
import Control.Cofree (Cofree, hoistCofree, mkCofree, tail)
import Control.Lazy (class Lazy)
import Control.ShiftMap (class ShiftMap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

----------
-- SIGNALS
----------

-- | Poor man's FRP implementation for Concur.
-- | I am experimenting with the smallest possible amount of FRP which can still be useful.
-- | A Widget can be considered to be a one-shot Event. (There is no stream of events in Concur).
-- | Signals then are never-ending widget loops that allow access to their last return value.
-- | This last produced value allows composition with other widgets even for never-ending widgets.
type Signal v a = Cofree (Widget v) a

-- | Construct a signal from an initial value, and a step widget
hold :: forall v a. a -> Widget v (Signal v a) -> Signal v a
hold = mkCofree

-- | A constant signal
always :: forall v a. Monoid v => a -> Signal v a
always a = hold a empty

-- | Update signal to a new value
update :: forall v a. Signal v a -> Widget v (Signal v a)
update = tail

-- | Construct a signal by polling a signal with a nested widget for values
poll :: forall v a. Signal v (Widget v a) -> Widget v (Signal v a)
poll b = hold <$> extract b <*> (map poll (update b))

-- | Create a signal which repeatedly invokes a widget for values.
-- | E.g. `signal False checkbox` will return a signal which reflects the current value of the checkbox.
signal :: forall v a. Monoid v => a -> Widget v a -> Signal v a
signal a = hold a <<< poll <<< always

-- | Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
loopW :: forall v a. a -> (a -> Widget v a) -> Signal v a
loopW a f = hold a (go <$> f a)
  where go x = loopW x f

-- | Loop a signal so that the return value is passed to the beginning again.
loopS :: forall v a. Monoid v => Show a => a -> (a -> Signal v a) -> Signal v a
loopS a f = hold (extract this) do
  s <- update this
  pure (loopS (extract s) f)
  where this = f a

-- | Folding signals. Similar to how signals used to work in Elm.
-- | This can be used to implement simple stateful Signals.
-- | e.g. `counter = fold (\n _ -> n+1) 0 clicks`
foldp :: forall v a b. (a -> b -> a) -> a -> Signal v b -> Signal v a
foldp f a sb = hold a' (map (foldp f a') (update sb))
  where a' = f a (extract sb)

-- | Consume a closed signal to make a widget
-- dyn :: forall v. (forall x. Signal v x) ~> (forall x. Widget v x)
dyn :: forall v a b. Signal v a -> Widget v b
dyn s = update s >>= dyn

-- | Run a signal *once* and return its value
oneShot :: forall v a. Signal v (Maybe a) -> Widget v a
oneShot s = case extract s of
  Nothing -> update s >>= oneShot
  Just a -> pure a

-- Sink the output from a signal into a widget
sink :: forall v a b. WidgetCombinator v -> (a -> Widget v b) -> Signal v a -> Widget v b
sink c f s = do
  e <- c (map Left (f (extract s))) (map Right (update s))
  case e of
    Left b -> pure b
    Right s' -> sink c f s'
