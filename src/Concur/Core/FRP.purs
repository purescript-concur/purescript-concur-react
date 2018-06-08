module Concur.Core.FRP where

import Prelude

import Concur.Core (Widget, WidgetCombinator)
import Control.Alternative ((<|>))
import Control.Comonad (class Comonad, class Extend, extract)
import Control.Comonad.Cofree (Cofree, hoistCofree, mkCofree, tail)
import Control.ShiftMap (class ShiftMap)
import Data.Either (Either(..))
import Data.Monoid (class Monoid)


----------
-- SIGNALS
----------

-- | Poor man's FRP implementation for Concur.
-- | I am experimenting with the smallest possible amount of FRP which can still be useful.
-- | A Widget can be considered to be a one-shot Event. (There is no stream of events in Concur).
-- | Signals then are never-ending widget loops that allow access to their last return value.
-- | This last produced value allows composition with other widgets even for never-ending widgets.
newtype Signal v a = Signal (Cofree (Widget v) a)

-- Signals allow us to have an FRP network!
-- But we need a custom bind instance

runSignal :: forall v a. Signal v a -> Cofree (Widget v) a
runSignal (Signal s) = s

derive newtype instance signalFunctor :: Functor (Signal v)
derive newtype instance signalExtend :: Extend (Signal v)
derive newtype instance signalComonad :: Comonad (Signal v)
derive newtype instance signalApplicative :: Monoid v => Applicative (Signal v)
derive newtype instance signalApply :: Monoid v => Apply (Signal v)

instance bindSignal :: Monoid v => Bind (Signal v) where
  bind fa f = go fa
    where
    go fa' =
      let fh = f (extract fa')
      in hold (extract fh) ((go <$> update fa') <|> (update fh))

instance monadSignal :: Monoid v => Monad (Signal v)

instance signalMonad :: Monoid v => Monad (Signal v)
-- derive newtype instance sigMonadRec :: Monoid v => MonadRec (Sig v)

instance shiftMapSignal :: ShiftMap (Widget v) (Signal v) where
  shiftMap f (Signal s) = Signal (hoistCofree f s)

-- | Construct a signal from an initial value, and a step widget
hold :: forall v a. a -> Widget v (Signal v a) -> Signal v a
hold a w = Signal (mkCofree a (map runSignal w))

-- | A constant signal
always :: forall v a. a -> Signal v a
always a = a `hold` (pure (always a))

-- | Update signal to a new value
update :: forall v a. Signal v a -> Widget v (Signal v a)
update s = map Signal (tail (runSignal s))

-- | Construct a signal by polling a signal with a nested widget for values
poll :: forall v a. Signal v (Widget v a) -> Widget v (Signal v a)
poll b = hold <$> extract b <*> (map poll (update b))

-- | Create a signal which repeatedly invokes a widget for values.
-- | E.g. `signal False checkbox` will return a signal which reflects the current value of the checkbox.
signal :: forall v a. a -> Widget v a -> Signal v a
signal a = hold a <<< poll <<< always

-- | Create a signal which repeatedly invokes a widget function for values, looping in the prev value.
loop :: forall v a. a -> (a -> Widget v a) -> Signal v a
loop a f = hold a (go <$> f a)
  where go x = loop x f

-- | Folding signals. Similar to how signals used to work in Elm.
-- | This can be used to implement simple stateful Signals.
-- | e.g. `counter = fold (\n _ -> n+1) 0 clicks`
foldp :: forall v a b. (a -> b -> a) -> a -> Signal v b -> Signal v a
foldp f a sb = hold a' (map (foldp f a') (update sb))
  where a' = f a (extract sb)

-- Consume a closed signal to make a widget
-- dyn :: forall v. (forall x. Signal v x) ~> (forall x. Widget v x)
dyn :: forall v a b. Signal v a -> Widget v b
dyn s = update s >>= dyn

-- Sink the output from a signal into a widget
sink :: forall v a b. WidgetCombinator v -> (a -> Widget v b) -> Signal v a -> Widget v b
sink c f s = do
  e <- c (map Left (f (extract s))) (map Right (update s))
  case e of
    Left b -> pure b
    Right s' -> sink c f s'
