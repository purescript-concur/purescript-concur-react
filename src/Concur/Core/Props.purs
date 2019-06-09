module Concur.Core.Props where

import Control.Applicative (pure)
import Data.Function ((<<<))
import Data.Functor (class Functor)
import Data.Unit (Unit, unit)
import Effect (Effect)

data Props p a
  = PrimProp p
  | Handler ((a -> Effect Unit) -> p)

instance functorProps :: Functor (Props p) where
  map _ (PrimProp p) = PrimProp p
  map f (Handler h) = Handler \k -> h (k <<< f)

-- | Internal. Do not use. Use unsafeMkProp, or unsafeMkPropHandler instead.
mkProp
  :: forall p a
  . (a -> Effect Unit)
  -> Props p a
  -> p
mkProp _ (PrimProp a) = a
mkProp h (Handler f) = f h

-- | Use `handleProp` to handle an event manually
handleProp
  :: forall p a b
  .  (a -> Effect Unit)
  -> Props p a
  -> Props p b
handleProp _ (PrimProp p) = PrimProp p
handleProp f (Handler g) = PrimProp (g f)

-- | Use this to filter the output of an event handler prop.
-- | For example, to only handle the enter key - `filterProp isEnterEvent onKeyDown`
filterProp ::
  forall p a.
  (a -> Boolean) ->
  Props p a ->
  Props p a
filterProp _ p@(PrimProp _) = p
filterProp ok (Handler g) = Handler \h ->
  (g \a -> if ok a
      then h a
      else pure unit)
