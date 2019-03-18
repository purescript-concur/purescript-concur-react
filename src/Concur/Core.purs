module Concur.Core
( module Concur.Core.Types
, module Concur.Core
)
where

import Concur.Core.Discharge (discharge)
import Concur.Core.Types (Widget(..), WidgetStep(..), resume, unWidget)
import Control.Monad.Free (Free, liftF, wrap)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (alt)
import Data.Either (Either(..))
import Data.Function (const)
import Effect (Effect)
import Effect.AVar (empty, tryPut) as EVar
import Effect.Aff.AVar (take) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import Prelude (class Monoid, Unit, bind, map, pure, show, void, ($), (<<<), (<>))

-- Helpers for some very common use of unsafe blocking io

-- | Construct a widget from a primitive view event
withViewEvent ::
  forall a v.
  ((a -> Effect Unit) -> v) ->
  Widget v a
withViewEvent mkView = Widget (liftF (WidgetStep (do
  v <- EVar.empty
  pure (Right { view: mkView (\a ->
                void (EVar.tryPut a v))
              , cont: liftAff (AVar.take v)
              }))))

-- | Construct a widget, by wrapping an existing widget in a view event
wrapViewEvent ::
  forall a v.
  ((a -> Effect Unit) -> v -> v) ->
  Widget v a ->
  Widget v a
wrapViewEvent mkView (Widget w) = Widget (wrapViewEvent' mkView w)

-- Private
wrapViewEvent' :: forall a v. ((a -> Effect Unit) -> v -> v) -> Free (WidgetStep v) a -> Free (WidgetStep v) a
wrapViewEvent' mkView w = case resume w of
  Left a -> pure a
  Right (WidgetStep wsm) -> wrap $ WidgetStep do
    ews <- wsm
    case ews of
      Left a -> pure (Left a)
      Right ws -> do
        var <- EVar.empty
        let eventHandler = (\a -> void (EVar.tryPut (pure a) var))
        let cont' = sequential (alt (parallel (liftAff (AVar.take var)))
                                    (parallel (map (wrapViewEvent' mkView) ws.cont))
                               )
        pure (Right { view: mkView eventHandler ws.view
                    , cont: cont'
                    })

-- | Construct a widget, by wrapping a widget constructor function in a view event
wrapViewEventFunc ::
  forall a v x. Monoid v =>
  ((a -> Effect Unit) -> (x -> Effect v) -> v) ->
  -- Don't want to pollute the API with `x -> Effect (Widget v a)`
  (x -> Widget v a) ->
  Widget v a
wrapViewEventFunc mkView mkWidget = Widget $ wrap $ WidgetStep do
  var <- EVar.empty
  let eventHandler = (\a -> void (EVar.tryPut (pure a) var))
  let handler (Left err) = log ("FAILED ROUTE WIDGET " <> show err)
      handler (Right w) = void (EVar.tryPut (wrapViewEvent' mkView' (unWidget w)) var)
  pure (Right { view: mkView eventHandler (discharge handler <<< mkWidget)
              , cont: AVar.take var
              })
  where
  mkView' h v = mkView h (const (pure v))

-- | Construct a widget with just props
mkLeafWidget ::
  forall a v.
  ((a -> Effect Unit) -> v) ->
  Widget v a
mkLeafWidget mkView = Widget $ wrap $ WidgetStep do
  var <- EVar.empty
  let view' = mkView (\a ->
        void (EVar.tryPut (pure a) var))
  let cont' = liftAff (AVar.take var)
  pure (Right { view: view', cont: cont' })
