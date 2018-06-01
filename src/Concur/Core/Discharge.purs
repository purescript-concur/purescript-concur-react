module Concur.Core.Discharge where

import Prelude

import Concur.Core (Widget(..), WidgetStep(..))
import Control.Monad.Aff (Milliseconds(..), delay, runAff_)
import Control.Monad.Aff.AVar (putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.AVar (makeEmptyVar, tryTakeVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (resume)
import Control.Monad.IO (IO, launchIO, runIO')
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)

-- Widget discharge strategies

-- | Discharge a widget.
-- | 1. Runs the sync IO action
-- | 2. Forks the async IO action
-- | 3. Extracts and returns the view
discharge :: forall a v. Monoid v
          => (Either Error (Widget v a) -> IOSync Unit)
          -> Widget v a
          -> IOSync v
discharge handler (Widget w) = case resume w of
  Right _ -> pure mempty
  Left (WidgetStep mws) -> do
    ws <- mws
    liftEff $ runAff_ (runIOSync <<< handler <<< map Widget) $ runIO' ws.cont
    pure ws.view

-- | Discharge a widget, forces async resolution of the continuation.
-- | 1. Runs the sync IO action
-- | 2. Forks the async IO action, using an async delay to guarantee that handler will not be called synchronously.
-- | 3. Extracts and returns the view
dischargeAsync :: forall a v. Monoid v
          => (Either Error (Widget v a) -> IOSync Unit)
          -> Widget v a
          -> IOSync v
dischargeAsync handler (Widget w) = case resume w of
  Right _ -> pure mempty
  Left (WidgetStep mws) -> do
    ws <- mws
    liftEff $ runAff_ (runIOSync <<< handler <<< map Widget) do
      delay (Milliseconds 0.0)
      a <- runIO' ws.cont
      pure a
    pure ws.view

-- | Discharge a sync widget.
-- | 1. Runs the sync IO action
-- | 2. Tries to run the async IO action without forking
-- |    If it succeeds, then it returns (Left <result>)
-- |    If it cannot be done, then it returns (Right <remaining IO action>)
-- | 3. Extracts and returns the view
dischargeSync :: forall a v. Monoid v
          => (Either Error (Widget v a) -> IOSync Unit)
          -> Widget v a
          -> IOSync v
dischargeSync handler (Widget winit) = go winit
  where
    go w = case resume w of
      Right _ -> pure mempty
      Left (WidgetStep mws) -> do
        ws <- mws
        res <- ioToIosync ws.cont
        case res of
          Left w' -> go w'
          Right io -> do
            liftEff $ runAff_ (runIOSync <<< handler <<< map Widget) $ runIO' io
            pure ws.view

-- UTIL
-- Potentially early resolve an IO computation (if it can be run synchronously)
-- if so, return (Left result), else return (Right IO)
ioToIosync :: forall a. IO a -> IOSync (Either a (IO a))
ioToIosync io = do
  v <- liftEff makeEmptyVar
  launchIO do
    a <- io
    liftAff (putVar a v)
  ma <- liftEff (tryTakeVar v)
  case ma of
    Nothing -> pure (Right (liftAff (takeVar v)))
    Just a -> pure (Left a)
