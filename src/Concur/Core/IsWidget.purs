module Concur.Core.IsWidget where

-- | A Nice way to bring into scope all needed functionality which we need to write widgets
import Concur.Core.LiftWidget (class LiftWidget)
import Concur.Core.Types (Widget)
import Control.Monad (class Monad)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Monoid (class Monoid)

-- | A Nice way to bring into scope all needed functionality which we need to write widgets
class (Monad m, Monoid v, ShiftMap (Widget v) m, LiftWidget v m, MultiAlternative m) <= IsWidget v m

-- Trivial self instance
instance widgetIsWidget :: Monoid v => IsWidget v (Widget v)
