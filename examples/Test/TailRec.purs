module Test.TailRec where

import Prelude

import Concur.Core (Widget, pulse)
import Concur.React (HTML)
import Concur.React.DOM (div', p', text)
import Concur.React.Widgets (textButton')

-- How many iterations to run at a time
maxIterations :: Int
maxIterations = 10000

tailRecDemo :: forall a. Widget HTML a
tailRecDemo = do
  div'[ p' [text ("Clicking this button will perform " <> show maxIterations <> " iterations via tail recursion ")]
      , p' [text "Once done, you can restart the iterations as many times you want."]
      , do
          textButton' "Start Tail Recursion Demo"
          tailRecWidget 0 2
      ]

tailRecWidget :: forall a. Int -> Int -> Widget HTML a
tailRecWidget count times = do
  let newCount = count + 1
  if newCount > maxIterations
     then do
       textButton' ("Ran " <> show count <> " iterations. Restart? (n = " <> show times <> ")")
       tailRecWidget 0 (times + 1)
     else do
       -- For the first maxIterations times, tailRecWidget calls itself in a tight loop
       -- This would blow the stack, if it weren't for the pulse here.
       pulse
       tailRecWidget newCount times
