module Test.Signals where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, display, dyn, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (onClick)

import Concur.Core.Dado as Da

-- Counting buttons
clicks :: Int -> Signal HTML Int
clicks init = loopW init $ \n ->
  D.div_ Da.do
    n+1 <$ (D.button [onClick] $ D.text ("Increment this count -> " <> show n))
    n-1 <$ (D.button [onClick] $ D.text ("Decrement this count -> " <> show n))

-- Add an outer display
-- Composing a signal even after looping
countingWidget :: forall a. Widget HTML a
countingWidget = dyn $ loopS 0 \n -> do
  display (D.text (show n))
  clicks n
