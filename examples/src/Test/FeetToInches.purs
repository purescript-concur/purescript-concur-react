module Test.FeetToInches where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Bind ((>>=))
import Control.Monad (class Monad)
import Data.CommutativeRing ((*), (+))
import Data.EuclideanRing ((-), (/))
import Data.Functor (($>), (<$>))
import Data.Semigroup ((<>))
import Data.Show (show)

-- A bidirectional form which can convert between feet and inches (and vice versa)
feetToInches :: forall a. Widget HTML a
feetToInches = loopState widget 0
  where
  widget inches = D.div'
    [ (_*12) <$> counter "feet" (inches / 12)
    , counter "inches" inches
    ]

counter :: String -> Int -> Widget HTML Int
counter label count = D.div'
  [ D.p' [D.text (label <> ": " <> show count)]
  , D.button [P.onClick] [D.text "Increment"] $> count+1
  , D.button [P.onClick] [D.text "Decrement"] $> count-1
  ]

-- Util: Never ending widgets with localised state
loopState :: forall m a s. Monad m => (s -> m s) -> s -> m a
loopState f s = f s >>= loopState f
