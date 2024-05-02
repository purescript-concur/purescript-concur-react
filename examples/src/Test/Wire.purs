module Test.Wire where

import Prelude

import Data.Tuple (Tuple(..))

import Concur.Core (Widget)
import Concur.Core.Patterns (Wire, with, local, send, mapWire)
import Data.Lens as L
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Effect.Class (liftEffect)
import Effect.Console (log)
import Control.Alternative (empty)

counter :: forall b. Wire (Widget HTML) Int -> Widget HTML b
counter wire = with wire \x -> do
  if x >= 10
   then D.text "10"
   else do
     void $ D.button [ P.onClick ] [ D.text $ show x ]
     send wire $ x + 1
     empty

wireWidget :: forall a. Widget HTML a
wireWidget = local (Tuple 0 0) \wire -> D.div []
  [ D.div' [D.text "This counter is independent of the other two "]
  , counter (mapWire L.first wire)
  , D.div' [D.text "These two counters have the same state"]
  , counter (mapWire L.second wire)
  , counter (mapWire L.second wire)
  ]

