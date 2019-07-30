module Test.Wire where

import Concur.Core.Patterns (Wire, local, with)
import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Function (($))
import Data.Functor (void)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Prelude ((+), (<))

counter :: Wire Int -> Int -> Widget HTML Unit
counter k x = do
  void $ D.button [ P.onClick ] [ D.text $ show x ]
  if x < 10
    then with k $ x + 1
    else pure unit

counterWithMessage :: forall a. Wire Int -> Int -> Widget HTML a
counterWithMessage k x = do
  counter k x
  D.div [] [ D.text "Counter finished" ]

wireWidget :: forall a. Widget HTML a
wireWidget = local 0 \k x -> D.div []
  [ counterWithMessage k x
  , counterWithMessage k x
  ]
