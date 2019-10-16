module Concur.Core.Patterns where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus, empty, (<|>))
import Data.Either (Either(..), either)
import Data.Lens (Lens')
import Data.Lens as L
import Data.Tuple (Tuple(..))
import Effect.AVar as EVar
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

-- | A very useful combinator for widgets with localised state
loopState ::
  forall m a s.
  Monad m =>
  s ->
  (s -> m (Either s a)) ->
  m a
loopState s f = f s >>= case _ of
  Left s' -> loopState s' f
  Right a -> pure a

-- | Repeat a computation until the value satisfies a predicate
retryUntil :: forall m a. Monad m => (a -> Boolean) -> m a -> m a
retryUntil p w = w >>= \a -> if p a then pure a else retryUntil p w

-- | Repeat a computation until the value satisfies a predicate, looping in the previous value
retryUntilLoop :: forall m a. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
retryUntilLoop p w a = w a >>= \a' -> if p a' then pure a' else retryUntilLoop p w a'

-- | The Elm Architecture
tea ::
  forall a s m x.
  Monad m =>
  s ->
  (s -> m a) ->
  (a -> s -> s) ->
  m x
tea s render update = go s
  where
  go st = render st >>= (flip update st >>> go)

-- | Separate the effect of the widget from its result
remoteWidget ::
  forall m n a void.
  MonadEffect n =>
  MonadAff m =>
  MonadEffect m =>
  Plus m =>
  m a ->
  n (Tuple (m a) (m void))
remoteWidget axn = do
  var <- liftEffect $ EVar.empty
  pure $ Tuple (liftAff (AVar.take var)) do
    f <- axn
    _ <- liftEffect $ EVar.tryPut f var
    empty

-- | A common pattern - running a long running action and keeping the GUI responsive
-- | Because the action can't be restarted on every gui event, we must *fork* it off in the beginning
forkAction ::
  forall m a b.
  MonadEffect m =>
  MonadAff m =>
  Plus m =>
  m a ->
  (m a -> m b) ->
  m b
forkAction axn rest = do
  Tuple axn' background <- remoteWidget axn
  background <|> rest axn'

-- | Another common variant on the `forkAction` pattern.
-- |   The action `m (s->s)` may take a while (should not be restarted) and returns a state modification function
-- |   The gui `s -> m s` takes in the current state, and modifies it on events
-- | Note that `forkActionState axn` has the shape `(s -> m s) -> (s -> m s)`. So it can be "stacked" to fork multiple actions.
-- | e.g. `forkActionState axn1 $ forkActionState axn2 $ forkActionState axn3 $ render initialState`.
forkActionState ::
  forall m s.
  Plus m =>
  MonadAff m =>
  m (s -> s) ->
  (s -> m s) ->
  (s -> m s)
forkActionState axn render st = forkAction axn (go st)
  where
  go st' axn' = do
    e <- (Left <$> render st') <|> (Right <$> axn')
    case e of
      Left st'' -> go st'' axn'
      Right f -> render (f st')


-- WORKING WITH LOCAL ENVIRONMENTS

-- | A wire can send values up into a local environment
type Wire m a = { value :: a, send :: a -> m Void, receive :: m a }

-- | Map a Lens over a Wire
mapWire :: forall m s a. Functor m => Lens' s a -> Wire m s -> Wire m a
mapWire lens wire =
  { value: L.view lens wire.value
  , send: \a -> wire.send $ L.set lens a wire.value
  , receive: map (L.view lens) wire.receive
  }

-- | Setup a local environment with a wire
local :: forall m r a. Alt m => MonadEffect m => MonadAff m => Plus m => a -> (Wire m a -> m r) -> m r
local a f = do
  var <- liftEffect EVar.empty
  go { value: a
     , send: \a' -> liftAff (AVar.put a' var) *> empty
     , receive: liftAff $ AVar.take var
     }
  where
  updateWire wire a' = wire {value=a'}
  go wire = do
    res <- (Left <$> f wire) <|> (Right <$> wire.receive)
    either pure (go <<< updateWire wire) res
