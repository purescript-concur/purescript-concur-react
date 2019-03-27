module Control.MultiAlternative where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.RWS.Trans (RWST(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))
import Control.Plus (class Plus, empty, alt)
import Data.Foldable (foldl)
import Data.Newtype (unwrap)

class Plus m <= MultiAlternative m where
  orr :: forall a. Array (m a) -> m a

-- Use this if there isn't a more efficient implementation
defaultOrr ::
  forall a m.
  Plus m =>
  Array (m a) ->
  m a
defaultOrr xs = foldl alt empty xs

-- TODO: Define a fallback instance for all monad transformers
-- instance multiAlternativeMonadTrans :: (Plus (t m), MonadTrans t) => MultiAlternative (t m) where
--   orr = defaultOrr

-- Instances for common transformers
instance exceptMultiAlternative ::
  ( Monoid e
  , MultiAlternative m
  , Monad m
  ) =>
  MultiAlternative (ExceptT e m) where
  -- `Alt` instance for ExceptT executes actions one at a time (as it should),
  -- Hence there is no way (or need) to use the efficient `orr` from the underlying monad
  -- So we can use the default implementation
  orr = defaultOrr

instance rwsMultiAlternative ::
  ( MultiAlternative m
  ) =>
  MultiAlternative (RWST r w s m) where
  orr rs = RWST \r s -> orr (map (\m -> unwrap m r s) rs)

instance readerMultiAlternative ::
  ( MultiAlternative m
  ) =>
  MultiAlternative (ReaderT r m) where
  orr rs = ReaderT \r -> orr (map (\m -> unwrap m r) rs)

instance stateMultiAlternative ::
  ( MultiAlternative m
  , Monad m
  ) =>
  MultiAlternative (StateT s m) where
  orr rs = StateT \s -> orr (map (\m -> unwrap m s) rs)

instance writerMultiAlternative ::
  ( MultiAlternative m
  ) =>
  MultiAlternative (WriterT s m) where
  orr rs = WriterT $ orr $ map unwrap rs
