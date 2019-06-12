module Concur.Core.Dado where

-- Dado: n. a groove or rectangular section in a board for receiving the end of another board
-- A Dado joins multiple UI elements together at the same time.
-- This is usually done with lists in Concur, but a Dado allows using do-notation.

import Control.Alt (class Alt, alt)
import Control.Applicative (class Applicative)
import Control.Applicative as P
import Data.Unit (Unit, unit)

pure ∷ forall m a. Applicative m => a -> m a
pure = P.pure

bind ∷ forall m a. Alt m => m a -> (Unit -> m a) -> m a
bind m f = alt m (f unit)

discard :: forall m a. Alt m => m a -> (Unit -> m a) -> m a
discard = bind
