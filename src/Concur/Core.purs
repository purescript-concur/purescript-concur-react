module Concur.Core where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Aff (Aff, never, runAff_)
import Control.Monad.Aff.AVar (takeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (makeEmptyVar, tryPutVar)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free, hoistFree, liftF, resume, wrap)
import Control.Monad.IO (IO, runIO')
import Control.Monad.IOSync (IOSync, runIOSync)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Alt, class Plus, alt, empty)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, updateAt)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup.Foldable (foldMap1)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

type WidgetStepRecord v a =
  { view :: v
  , cont :: IO a
  }

newtype WidgetStep v a = WidgetStep (IOSync (WidgetStepRecord v a))

unWidgetStep :: forall v a. WidgetStep v a -> IOSync (WidgetStepRecord v a)
unWidgetStep (WidgetStep x) = x

instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStep w) = WidgetStep (map mod w)
    where mod ws = ws { cont = map f ws.cont }

displayStep :: forall a v. v -> WidgetStep v a
displayStep v = WidgetStep (pure { view: v, cont: liftAff never })

newtype Widget v a = Widget (Free (WidgetStep v) a)

unWidget :: forall v a. Widget v a -> Free (WidgetStep v) a
unWidget (Widget w) = w

derive newtype instance widgetFunctor :: Functor (Widget v)
derive newtype instance widgetBind :: Bind (Widget v)
derive newtype instance widgetApplicative :: Applicative (Widget v)
derive newtype instance widgetApply :: Apply (Widget v)
instance widgetMonad :: Monad (Widget v)

-- | Discharge a widget.
-- | 1. Forks the async IO action
-- | 2. Runs the sync IO action
-- | 3. Extracts and returns the view
discharge :: forall a v. Monoid v
          => Widget v a
          -> (Either Error (Widget v a) -> IOSync Unit)
          -> IOSync v
discharge (Widget w) handler = case resume w of
  Right _ -> pure mempty
  Left (WidgetStep mws) -> do
    ws <- mws
    liftEff $ runAff_ (runIOSync <<< handler <<< map Widget) $ runIO' ws.cont
    pure ws.view

-- Util
flipEither :: forall a b. Either a b -> Either b a
flipEither (Left a) = Right a
flipEither (Right b) = Left b

-- This instance is more efficient than `defaultOrr` for a large number of widgets
instance widgetMultiAlternative :: Monoid v => MultiAlternative (Widget v) where
  orr wss = case fromArray wss of
    Just wsne -> Widget $ comb $ map unWidget wsne
    Nothing -> empty
    where
      comb :: forall v' a. Monoid v'
           => NonEmptyArray (Free (WidgetStep v') a)
           -> Free (WidgetStep v') a
      -- Use flipEither so `Left` indicates a return value, which we can short circuit on
      -- 'Either.sequence'
      comb wfs = case sequence (map (flipEither <<< resume) wfs) of
        Left a -> pure a
        Right wsm -> wrap $ WidgetStep do
            -- 'IOSync.sequence'
            ws <- sequence $ map unWidgetStep wsm
            pure { view: foldMap1 _.view ws
                 , cont: merge ws (map _.cont ws)
                 }

      merge :: forall v' a. Monoid v'
          => NonEmptyArray (WidgetStepRecord v' (Free (WidgetStep v') a))
          -> NonEmptyArray (IO (Free (WidgetStep v') a))
          -> IO (Free (WidgetStep v') a)
      merge ws wscs = do
        -- wsm' is discharged wsm, with all the IO action run exactly once
        let wsm = map (wrap <<< WidgetStep <<< pure) ws
        -- TODO: We know the array is non-empty. We need something like foldl1WithIndex.
        Tuple i e <- sequential (foldlWithIndex (\i r w -> alt (parallel (map (Tuple i) w)) r) empty wscs)
        -- TODO: All the IO in ws is already discharged. Use a more efficient way than comb to process it
        -- TODO: Also, more importantly, we would like to not have to cancel running fibers unless one of them returns a result
        pure $ comb (fromMaybe wsm (updateAt i e wsm))

instance widgetSemigroup :: Monoid v => Semigroup (Widget v a) where
  append w1 w2 = orr [w1, w2]

instance widgetAlt :: Monoid v => Alt (Widget v) where
  alt = append

instance widgetPlus :: Monoid v => Plus (Widget v) where
  empty = display mempty

instance widgetAlternative :: Monoid v => Alternative (Widget v)

mapView :: forall a v. (v -> v) -> Widget v a -> Widget v a
mapView f (Widget w) = Widget (hoistFree (mapViewStep f) w)

mapViewStep :: forall v1 v2 a. (v1 -> v2) -> WidgetStep v1 a -> WidgetStep v2 a
mapViewStep f (WidgetStep ws) = WidgetStep (map mod ws)
  where mod ws' = ws' { view = f ws'.view }

display :: forall a v. v -> Widget v a
display v = Widget (liftF (displayStep v))

-- Sync but Non blocking eff
effAction :: forall a v eff. v -> Eff eff a -> Widget v a
effAction v eff = affAction v $ liftEff eff

-- Sync and blocking eff
-- WARNING: UNSAFE: This will block the UI rendering
unsafeBlockingEffAction :: forall a v eff. v -> Eff eff a -> Widget v a
unsafeBlockingEffAction v eff = Widget $ liftF $ WidgetStep $
  liftEff eff >>= \a -> pure { view: v, cont: pure a }

-- Async aff
affAction :: forall a v eff. v -> Aff eff a -> Widget v a
affAction v aff = Widget $ liftF $ WidgetStep do
  var <- liftEff do
    var <- makeEmptyVar
    runAff_ (handler var) (unsafeCoerceAff aff)
    pure var
  pure { view: v, cont: liftAff (takeVar var) }
  where
    handler _   (Left e) = log ("Aff failed - " <> show e)
    handler var (Right a) = void (tryPutVar a var)

instance widgetMonadEff :: Monoid v => MonadEff eff (Widget v) where
  liftEff = effAction mempty

instance widgetMonadAff :: Monoid v => MonadAff eff (Widget v) where
  liftAff = affAction mempty

-- Helpers for some very common use of unsafe blocking io

-- Construct a widget from a primitive view event
withViewEvent :: forall a v. ((a -> IOSync Unit) -> v) -> Widget v a
withViewEvent mkView = Widget (liftF (WidgetStep (do
     v <- liftEff makeEmptyVar
     pure { view: mkView (\a -> void (liftEff (tryPutVar a v))), cont: liftAff (takeVar v) }
  )))

-- Construct a widget, by wrapping an existing widget in a view event
-- Returns Left on view event firing, Right on wrapped widget finishing
wrapViewEvent :: forall a b v. ((a -> IOSync Unit) -> v -> v) -> Widget v b -> Widget v (Either a b)
wrapViewEvent mkView (Widget w) = Widget $
  case resume w of
    Right a -> pure (Right a)
    Left (WidgetStep wsm) -> wrap $ WidgetStep do
      ws <- wsm
      var <- liftEff makeEmptyVar
      let view' = mkView (\a -> void (liftEff (tryPutVar (pure (Left a)) var))) ws.view
      let cont' = sequential (alt (parallel (liftAff (takeVar var))) (parallel (map (map Right) ws.cont)))
      pure {view: view', cont: cont'}
