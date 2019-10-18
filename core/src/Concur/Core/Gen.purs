module Concur.Core.Gen where

import Prelude

import Concur.Core.Types (Widget)
import Control.Alt ((<|>))
import Control.Monad.Free (Free, hoistFree, liftF, resume, wrap)
import Data.Array (foldr, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Internal data types
newtype GenStep v x a
  = GenStep (GenWidget v x a)

type GenWidget v x a
  = Widget v {yield :: Maybe x, cont :: a}

instance functorGenStep :: Functor (GenStep v x) where
  map f (GenStep w) = GenStep (mapContGenWidget f w)

-- | A Gen is a widget that also generates things
type Gen v x a
  = Free (GenStep v x) a

-- | Sometimes it's useful to have Generators that generate Widgets
type WidgetGen v b a
  = Gen v (Widget v b) a

-- | Yield a value
yield ::
  forall v x.
  x ->
  Gen v x Unit
yield = liftF <<< pureYield

-- | Run a Widget
runWidget ::
  forall v x a.
  Widget v a ->
  Gen v x a
runWidget = liftF <<< widgetYield Nothing

-- | Yield a value, and then continue
yieldAndThen ::
  forall v x a.
  x ->
  Widget v (Gen v x a) ->
  Gen v x a
yieldAndThen x = wrap <<< widgetYield (Just x)

-- | A map over yielded values (of type X)
-- | The usual map is over the return value
mapYield ::
  forall v x y a.
  (x -> y) ->
  Gen v x a ->
  Gen v y a
mapYield f = hoistFree (\(GenStep w) ->
  GenStep (mapYieldGenWidget f w))

-- | Convert a generator into one that tags its output with successive unique integers
zipYield ::
  forall v x a.
  Gen v x a ->
  Gen v (Tuple Int x) a
zipYield = go 0
  where
  go n g = case resume g of
    Right b -> pure b
    Left (GenStep gw) -> do
      r <- runWidget gw
      case r.yield of
        Nothing -> go n r.cont
        Just x -> yieldAndThen (Tuple n x) $ pure $ go (n + 1) r.cont

-- | Convert a monadic generator into one that tags its output with successive unique integers
-- | Can also be specialised to :: WidgetGen v x a -> WidgetGen v (Tuple Int x) a
zipWidgetYield ::
  forall a v m x.
  Functor m =>
  Gen v (m x) a ->
  Gen v (m (Tuple Int x)) a
zipWidgetYield g = mapYield (\(Tuple x w) ->
  Tuple x <$> w) $ zipYield g

-- TODO: A Widget when generated and injected into a container by a generator,
--       should have some mechanism to dictate its position.
-- TODO: Actually, we need a monad independent layout format.
--       An idea is - view = mapping from { selector -> Widget },
--         where type of selector depends on type of view.
-- | Collapse a Generator into one widget. For containers with dynamic children.
-- | Any new widgets generated are immediately inserted into the parent widget
-- | Returns either (Left b) when Gen ends, or Right a, when one of the children end.
genOrr ::
  forall v a b.
  Monoid v =>
  WidgetGen v a b ->
  Widget v (Either b a)
genOrr wg = case resume wg of
  Right b -> pure (Left b)
  Left (GenStep gw) -> do
    r <- gw
    case r.yield of
      Nothing -> genOrr r.cont
      Just x -> genOrr r.cont <|> (Right <$> x)

-- | Like `genOrr`, collapses a Generator into one widget.
-- | However, any values returned by the children are tagged with an id (unique to this generator)
-- | Any new widgets generated are immediately inserted into the parent widget
-- | Returns either (Left b) when Gen ends, or (Tuple Int a), when one of the children end.
zipGenOrr ::
  forall v a b.
  Monoid v =>
  WidgetGen v a b ->
  Widget v (Either b (Tuple Int a))
zipGenOrr = genOrr <<< zipWidgetYield

-- | Array to Generator conversion
-- | Sequentially generates all values in the list
listToGen ::
  forall v x.
  Array x ->
  Gen v x Unit
listToGen = foldr (bthen <<< yield) (pure unit)
  where
  bthen m1 m2 = m1 >>= \_ ->
    m2

-- | Generator to Array conversion. Runs until generator ends, then returns all generated values in an array.
-- | Use it when you want to generate values, and then operate on them in one go
genToList ::
  forall v x a.
  Gen v x a ->
  Widget v (Array x)
genToList g = case resume g of
  Right _ -> pure []
  Left (GenStep gw) -> do
    r <- gw
    rs <- genToList r.cont
    pure case r.yield of
      Nothing -> rs
      Just x -> snoc rs x

-- Util
mapYieldGenWidget ::
  forall v x y a.
  (x -> y) ->
  GenWidget v x a ->
  GenWidget v y a
mapYieldGenWidget f = map (\r ->
  r { yield = map f r.yield })

mapContGenWidget ::
  forall v x a b.
  (a -> b) ->
  GenWidget v x a ->
  GenWidget v x b
mapContGenWidget f = map (\r ->
  r { cont = f r.cont })

pureYield :: forall v x. x -> GenStep v x Unit
pureYield x = GenStep (pure { yield: Just x, cont: unit })

widgetYield :: forall v x a. Maybe x -> Widget v a -> GenStep v x a
widgetYield mx w = GenStep do
  a <- w
  pure { yield: mx, cont: a }
