module Concur.Core.LiftWidget where

-- | A way to lift widgets into higher monads
import Concur.Core.Types (Widget)
import Control.Monad (class Monad)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Data.Function (identity, (<<<))
import Data.Monoid (class Monoid)

-- | A way to lift widgets into higher monads
class LiftWidget v m where
  liftWidget :: forall a. Widget v a -> m a

-- TODO: LiftWidget instance for all transformers
-- instance liftWidgetTrans :: (Monad m, MonadTrans t, LiftWidget v m) => LiftWidget v (t m) where
--   liftWidget = lift <<< liftWidget

-- Trivial self instance
instance widgetLiftWidget :: LiftWidget v (Widget v) where
  liftWidget = identity

-- Instances for common transformers
instance exceptLiftWidget :: (Monad m, LiftWidget v m) => LiftWidget v (ExceptT e m) where
  liftWidget = lift <<< liftWidget

instance rwsLiftWidget :: (Monoid w, Monad m, LiftWidget v m) => LiftWidget v (RWST r w s m) where
  liftWidget = lift <<< liftWidget

instance readerLiftWidget :: (Monad m, LiftWidget v m) => LiftWidget v (ReaderT r m) where
  liftWidget = lift <<< liftWidget

instance stateLiftWidget :: (Monad m, LiftWidget v m) => LiftWidget v (StateT s m) where
  liftWidget = lift <<< liftWidget

instance writerLiftWidget :: (Monoid s, Monad m, LiftWidget v m) => LiftWidget v (WriterT s m) where
  liftWidget = lift <<< liftWidget
