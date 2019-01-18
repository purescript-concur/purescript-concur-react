module Concur.Checkpoint where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)


checkpoint :: forall m a. MonadEffect m => String -> m a -> m a
checkpoint label w = do
  cachedVal <- liftEffect $ readCheckpoint label
  case cachedVal of
    Nothing -> do
      newVal <- w
      liftEffect $ writeCheckpoint label newVal
      pure newVal
    Just x -> do
      liftEffect $ incrProcessIndex label
      pure x

foreign import readCheckpoint :: forall a. String -> Effect (Maybe a)
foreign import writeCheckpoint :: forall a. String -> a -> Effect Unit
foreign import incrProcessIndex :: String -> Effect Unit
