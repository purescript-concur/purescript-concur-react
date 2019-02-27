module Concur.Core.DevTools where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)

import Effect.AVar as EVar
import Effect.Aff.AVar as AVar

data DevToolsConnection

foreign import connectDevTools :: Effect DevToolsConnection

foreign import disconnectDevTools :: Effect Unit

foreign import sendToDevTools :: forall action state. DevToolsConnection -> action -> state -> Effect Unit

data DevToolsSubscription

foreign import subscribeDevTools :: forall state. DevToolsConnection -> (state -> Effect Unit) -> Effect DevToolsSubscription

foreign import unsubscribeDevTools :: DevToolsSubscription -> Effect Unit

data StateSubscription a
  = StateSubscription DevToolsConnection DevToolsSubscription (EVar.AVar a)

subscribe :: forall a. DevToolsConnection -> Effect (StateSubscription a)
subscribe conn = do
  v <- EVar.empty
  subs <- subscribeDevTools conn \st ->
    do
      _ <- EVar.tryPut st v
      pure unit
  pure (StateSubscription conn subs v)

unsubscribe :: forall a. StateSubscription a -> Effect Unit
unsubscribe (StateSubscription _ subs v) = do
  unsubscribeDevTools subs
  EVar.kill (error "Unsubscribed") v

awaitState :: forall a. StateSubscription a -> Aff a
awaitState (StateSubscription _ _ v) = AVar.take v

sendState :: forall a. StateSubscription a -> String -> a -> Effect Unit
sendState (StateSubscription conn _ _) label st = sendToDevTools conn label st

-- Wrap a state getter, so that all outputs from the getter are sent to the devtools
--   And also, any state sent back from the devtools overrides the local state
withStateful ::
  forall m a.
  MonadAff m =>
  Alt m =>
  StateSubscription a ->
  String ->
  m a ->
  m a
withStateful subs label axn = do
  est <- map Left axn <|> map Right (liftAff (awaitState subs))
  case est of
    Left st -> do
      liftEffect $ sendState subs label st
      pure st
    Right st -> pure st
