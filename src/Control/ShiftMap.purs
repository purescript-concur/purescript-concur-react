module Control.ShiftMap where

import Prelude

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.RWS (RWST, mapRWST)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, mapWriterT)

-- | Mapping between Endo Natural Transformations
class ShiftMap s t where
  shiftMap :: (s ~> s) -> (t ~> t)

-- Instances for common transformers

instance exceptShiftMap :: ShiftMap m (ExceptT e m) where
  shiftMap = mapExceptT

instance rwsShiftMap :: ShiftMap m (RWST r w s m) where
  shiftMap = mapRWST

instance readerShiftMap :: ShiftMap m (ReaderT r m) where
  shiftMap = mapReaderT

instance stateShiftMap :: ShiftMap m (StateT s m) where
  shiftMap = mapStateT

instance writerShiftMap :: ShiftMap m (WriterT s m) where
  shiftMap = mapWriterT


-- Better than MonadTrans for my purposes
class ShiftUp s t where
  shiftUp :: s ~> t

instance exceptShiftUp :: Monad m => ShiftUp m (ExceptT e m) where
  shiftUp = lift

instance rwsShiftUp :: (Monoid w, Monad m) => ShiftUp m (RWST r w s m) where
  shiftUp = lift

instance readerShiftUp :: Monad m => ShiftUp m (ReaderT r m) where
  shiftUp = lift

instance stateShiftUp :: Monad m => ShiftUp m (StateT s m) where
  shiftUp = lift

instance writerShiftUp :: (Monad m, Monoid s) => ShiftUp m (WriterT s m) where
  shiftUp = lift
