module Concur.Core.Types where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Free (Free, hoistFree, liftF, resume', wrap)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Alt, class Plus, alt, empty)
import Control.ShiftMap (class ShiftMap)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromArray, updateAt)
import Data.Either (Either(..), either)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Semigroup.Foldable (foldMap1)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (empty, tryPut, tryTake) as EVar
import Effect.Aff (Aff, effectCanceler, makeAff, never, runAff_)
import Effect.Aff.AVar (take) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Exception (Error)

type WidgetStepRecord v a
  = {view :: v, cont :: Aff a}

newtype WidgetStep v a
  = WidgetStep (Effect (Either a (WidgetStepRecord v a)))

unWidgetStep ::
  forall v a.
  WidgetStep v a ->
  Effect (Either a (WidgetStepRecord v a))
unWidgetStep (WidgetStep x) = x

-- derive instance widgetStepFunctor :: Functor (WidgetStep v)
instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStep w) = WidgetStep (map mod w)
    where
    mod (Right ws) = Right (ws { cont = map f ws.cont
                               })
    mod (Left a) = Left (f a)

displayStep :: forall a v. v -> WidgetStep v a
displayStep v = WidgetStep (pure (Right { view: v, cont: never }))

newtype Widget v a
  = Widget (Free (WidgetStep v) a)

unWidget :: forall v a. Widget v a -> Free (WidgetStep v) a
unWidget (Widget w) = w

derive newtype instance widgetFunctor :: Functor (Widget v)

derive newtype instance widgetBind :: Bind (Widget v)

derive newtype instance widgetApplicative :: Applicative (Widget v)

derive newtype instance widgetApply :: Apply (Widget v)

instance widgetMonad :: Monad (Widget v)

derive newtype instance widgetMonadRec :: MonadRec (Widget v)

instance widgetShiftMap :: ShiftMap (Widget v) (Widget v) where
  shiftMap f = f identity

-- Util
flipEither ::
  forall a b.
  Either a b ->
  Either b a
flipEither (Left a) = Right a

flipEither (Right b) = Left b

resume :: forall f a. Functor f => Free f a -> Either a (f (Free f a))
resume = resume' (\g i ->
  Right (i <$> g)) Left

instance widgetMultiAlternative ::
  ( Monoid v
  ) =>
  MultiAlternative (Widget v) where
  orr wss = case fromArray wss of
    Just wsne -> Widget $ comb $ map unWidget wsne
    Nothing -> empty
    where
    comb ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (Free (WidgetStep v') a) ->
      Free (WidgetStep v') a
    -- If any sub-widget finished, then finish
    -- 'Either.traverse'
    comb wfs = case traverse resume wfs of
      Left a -> pure a
      Right wsm -> wrap $ WidgetStep do
        -- 'Effect.traverse'
        ewss <- traverse unWidgetStep wsm
        -- 'Effect.traverse'
        ews <- traverse stepWidget ewss
        -- Check if we got a result
        -- Either.sequence
        case sequence ews of
          -- I don't like rewrapping the result, might cause loops (such as https://github.com/ajnsit/purescript-concur/issues/16)
          -- But we needed to run the effects so we already committed to returning a widget continuation
          Left a -> pure (Left (pure a))
          Right ws -> pure $ Right
            { view: foldMap1 _.view ws
            , cont: merge ws (map _.cont ws)
            }
    -- Completely discharge the effect of the widget, until we get a result, or an aff
    stepWidget ::
      forall v' a.
      Monoid v' =>
      Either (Free (WidgetStep v') a) (WidgetStepRecord v' (Free (WidgetStep v') a)) ->
      Effect (Either a (WidgetStepRecord v' (Free (WidgetStep v') a)))
    stepWidget = either stepWidgetLeft stepWidgetRight
      where
      stepWidgetRight ws = pure (Right ws)
      stepWidgetLeft w = case resume w of
        Left a -> pure (Left a)
        Right (WidgetStep effws) -> effws >>= stepWidget

    merge ::
      forall v' a.
      Monoid v' =>
      NonEmptyArray (WidgetStepRecord v' (Free (WidgetStep v') a)) ->
      NonEmptyArray (Aff (Free (WidgetStep v') a)) ->
      Aff (Free (WidgetStep v') a)
    merge ws wscs = do
      -- wsm' is discharged wsm, with all the Aff action run exactly once
      let wsm = map (wrap <<< WidgetStep <<< pure <<< Right) ws
      -- TODO: We know the array is non-empty. We need something like foldl1WithIndex.
      Tuple i e <- sequential (foldlWithIndex (\i r w ->
        alt (parallel (map (Tuple i) w)) r) empty wscs)
      -- TODO: All the Aff in ws is already discharged. Use a more efficient way than comb to process it
      -- TODO: Also, more importantly, we would like to not have to cancel running fibers unless one of them returns a result
      pure $ comb (fromMaybe wsm (updateAt i e wsm))

-- | Run multiple widgets in parallel until *all* finish, and collect their outputs
-- | Contrast with `orr`
-- TODO: Performance? Don't orr with `empty`.
andd ::
  forall v a.
  Monoid v =>
  Array (Widget v a) ->
  Widget v (Array a)
andd ws = do
  Tuple i e <- foldrWithIndex (\i w r -> alt (map (Tuple i) w) r) empty ws
  let ws' = fromMaybe ws $ A.deleteAt i ws
  if A.length ws' <= 0
    then pure [e]
    else do
      rest <- andd ws'
      pure $ fromMaybe [] $ A.insertAt i e rest

instance widgetSemigroup :: (Monoid v) => Semigroup (Widget v a) where
  append w1 w2 = orr [w1, w2]

instance widgetMonoid :: (Monoid v) => Monoid (Widget v a) where
  mempty = empty

instance widgetAlt :: (Monoid v) => Alt (Widget v) where
  alt = append

instance widgetPlus :: (Monoid v) => Plus (Widget v) where
  empty = display mempty

instance widgetAlternative :: (Monoid v) => Alternative (Widget v)

-- Pause for a negligible amount of time. Forces continuations to pass through the trampoline.
-- (Somewhat similar to calling `setTimeout` of zero in Javascript)
-- Avoids stack overflows in (pathological) cases where a widget calls itself repeatedly without any intervening widgets or effects.
-- E.g. -
--   BAD  `counter n = if n < 10000 then counter (n+1) else pure n`
--   GOOD `counter n = if n < 10000 then (do pulse; counter (n+1)) else pure n`
pulse ::
  forall v.
  Monoid v =>
  Widget v Unit
pulse = effAction (pure unit)

mapView :: forall a v1 v2. (v1 -> v2) -> Widget v1 a -> Widget v2 a
mapView f (Widget w) = Widget (hoistFree (mapViewStep f) w)

mapViewStep :: forall v1 v2 a. (v1 -> v2) -> WidgetStep v1 a -> WidgetStep v2 a
mapViewStep f (WidgetStep ws) = WidgetStep (map mod ws)
  where
  mod = map (\ws' ->
    ws' { view = f ws'.view
        })

display :: forall a v. v -> Widget v a
display v = Widget (liftF (displayStep v))

-- Sync eff
effAction ::
  forall a v.
  Effect a ->
  Widget v a
effAction = Widget <<< liftF <<< WidgetStep <<< map Left

-- Async aff
affAction ::
  forall a v.
  v ->
  Aff a ->
  Widget v a
affAction v aff = Widget $ liftF $ WidgetStep do
  var <- EVar.empty
  runAff_ (handler var) aff
  -- Detect synchronous resolution
  ma <- EVar.tryTake var
  pure case ma of
    Just a -> Left a
    Nothing -> Right { view: v, cont: liftAff (AVar.take var) }
  where
  -- TODO: allow client code to handle aff failures
  handler _ (Left e) = log ("Aff failed - " <> show e)
  handler var (Right a) = void (EVar.tryPut a var)

-- Async callback
asyncAction ::
  forall v a.
  v ->
  ((Either Error a -> Effect Unit) -> Effect (Effect Unit)) ->
  Widget v a
asyncAction v handler = affAction v (makeAff (map effectCanceler <<< handler))

instance widgetMonadEff :: (Monoid v) => MonadEffect (Widget v) where
  liftEffect = effAction

instance widgetMonadAff :: (Monoid v) => MonadAff (Widget v) where
  liftAff = affAction mempty
