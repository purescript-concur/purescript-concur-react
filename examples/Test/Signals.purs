module Test.Signals where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loop, sink)
import Concur.React (HTML)
import Concur.React.DOM (div', text)
import Concur.React.Widgets (textButton')
import Control.Alternative ((<|>))

-- Counting buttons
clicks :: Signal HTML Int
clicks = loop 0 $ \n ->
  div' [ n+1 <$ textButton' ("Increment this count -> " <> show n)
       , n-1 <$ textButton' ("Decrement this count -> " <> show n)
       ]

-- Add an outer display
-- Composing a signal even after looping
countingWidget :: forall a. Widget HTML a
countingWidget = sink (<|>) (text <<< show) clicks

-- Display widget without an outer display
-- countingWidget :: forall a. Widget HTML a
-- countingWidget = dyn clicks
