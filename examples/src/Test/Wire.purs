module Test.Wire where

import Concur.Core.Patterns (Wire, local, mapWire)
import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Function (($))
import Data.Functor (void)
import Data.Lens as L
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Prelude ((+), (<))

counter :: Wire (Widget HTML) Int -> Widget HTML Unit
counter wire = do
  let x = wire.value
  void $ D.button [ P.onClick ] [ D.text $ show x ]
  if x < 10
    then wire.send $ x + 1
    else pure unit

counterWithMessage :: forall a. Wire (Widget HTML) Int -> Widget HTML a
counterWithMessage wire = do
  counter wire
  D.div [] [ D.text "Counter finished" ]

wireWidget :: forall a. Widget HTML a
wireWidget = local (Tuple 0 0) \wire -> D.div []
  [ D.div' [D.text "This counter is independent of the other two "]
  , counterWithMessage (mapWire L.first wire)
  , D.div' [D.text "These two counters have the same state"]
  , counterWithMessage (mapWire L.second wire)
  , counterWithMessage (mapWire L.second wire)
  ]
