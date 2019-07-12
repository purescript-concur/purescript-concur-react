module Test.Signals where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, display, dyn, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (button, div', text)
import Concur.React.Props (onClick)

-- Counting buttons
clicks :: Int -> Signal HTML Int
clicks init = loopW init $ \n ->
  div' [ n+1 <$ button [onClick] [text ("Increment this count -> " <> show n)]
       , n-1 <$ button [onClick] [text ("Decrement this count -> " <> show n)]
       ]

-- Add an outer display
-- Composing a signal even after looping
countingWidget :: forall a. Widget HTML a
countingWidget = dyn $ loopS 0 \n -> do
  display (text (show n))
  clicks n
