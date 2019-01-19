module Control.ShiftMap where

import Prelude

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.RWS (RWST, mapRWST)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State (StateT, mapStateT)
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
