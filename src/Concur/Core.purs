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
import Control.Monad.Free (Free, hoistFree, resume, liftF, wrap)
import Control.Monad.IO (IO)
import Control.Monad.IOSync (IOSync)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Alt, class Plus, alt, (<|>), empty)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Monoid (class Monoid, mempty)

newtype WidgetStep v a = WidgetStep (IOSync
  { view :: v
  , cont :: IO a
  })

unWidgetStep :: forall v a. WidgetStep v a -> IOSync { view :: v, cont :: IO a }
unWidgetStep (WidgetStep x) = x

instance functorWidgetStep :: Functor (WidgetStep v) where
  map f (WidgetStep w) = WidgetStep (map mod w)
    where mod ws = ws { cont = map f ws.cont }

displayStep :: forall a v. v -> WidgetStep v a
displayStep v = WidgetStep (pure { view: v, cont: liftAff never })

newtype Widget v a = Widget (Free (WidgetStep v) a)

unWidget :: forall v a. Widget v a -> Free (WidgetStep v) a
unWidget (Widget w) = w

instance widgetFunctor :: Functor (Widget v) where
  map k (Widget w) = Widget (map k w)

instance widgetBind :: Bind (Widget v) where
  bind (Widget w) f = Widget (bind w (unWidget <<< f))

instance widgetApplicative :: Applicative (Widget v) where
  pure = Widget <<< pure

instance widgetApply :: Apply (Widget v) where
  apply = ap

instance widgetMonad :: Monad (Widget v)

instance widgetSemigroup :: Semigroup v => Semigroup (Widget v a) where
  append (Widget w1) (Widget w2) = Widget (appendFree w1 w2)
    where
      appendFree w1' w2' =
        case resume w1' of
          Right a1 -> pure a1
          Left ws1 -> case resume w2' of
            Right a2 -> pure a2
            Left ws2 -> wrap (appendWidgetStep ws1 ws2)
      appendWidgetStep (WidgetStep wsm1) (WidgetStep wsm2) = WidgetStep $ do
        ws1 <- wsm1
        ws2 <- wsm2
        let v = ws1.view <> ws2.view
        let c = do
                  e <- sequential (alt (parallel (map Left ws1.cont)) (parallel (map Right ws2.cont)))
                  pure $ case e of
                      -- Taking care to not run any of the effects again
                      Left  e' -> appendFree e' (wrap (WidgetStep (pure ws2)))
                      Right e' -> appendFree (wrap (WidgetStep (pure ws1))) e'
        pure { view: v, cont: c }

instance widgetAlt :: Semigroup v => Alt (Widget v) where
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

orr :: forall m a. Plus m => Array (m a) -> m a
orr = foldl (<|>) empty

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
affAction v aff = Widget $ liftF $ WidgetStep $ do
  var <- liftEff $ do
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
    Left (WidgetStep wsm) -> wrap $ WidgetStep $ do
      ws <- wsm
      var <- liftEff makeEmptyVar
      let view' = mkView (\a -> void (liftEff (tryPutVar (pure (Left a)) var))) ws.view
      let cont' = sequential (alt (parallel (liftAff (takeVar var))) (parallel (map (map Right) ws.cont)))
      pure {view: view', cont: cont'}
