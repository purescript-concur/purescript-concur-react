module Control.MultiAlternative where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Plus (class Plus, empty, alt)
import Data.Foldable (foldl)

class Plus m <= MultiAlternative m where
  orr :: forall a. Array (m a) -> m a

-- Use this if there isn't a more efficient implementation
defaultOrr :: forall a m. Plus m => Array (m a) -> m a
defaultOrr xs = foldl alt empty xs

-- Instances for common transformers

-- TODO: Define an instance for all monad transformers
-- instance multiAlternativeMonadTrans :: (Plus (t m), MonadTrans t) => MultiAlternative (t m) where
--   orr = defaultOrr

instance exceptMultiAlternative :: (Monoid e, MultiAlternative m, Monad m) => MultiAlternative (ExceptT e m) where
  orr = defaultOrr

instance rwsMultiAlternative :: MultiAlternative m => MultiAlternative (RWST r w s m) where
  orr = defaultOrr

instance readerMultiAlternative :: MultiAlternative m => MultiAlternative (ReaderT r m) where
  orr = defaultOrr

instance stateMultiAlternative :: (MultiAlternative m, Monad m) => MultiAlternative (StateT s m) where
  orr = defaultOrr

instance writerMultiAlternative :: MultiAlternative m => MultiAlternative (WriterT s m) where
  orr = defaultOrr
