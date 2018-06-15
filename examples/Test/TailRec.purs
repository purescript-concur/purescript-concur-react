module Test.TailRec where

import Prelude

import Concur.Core (Widget, pulse)
import Concur.React (HTML)
import Concur.React.DOM (button, div', p', text)
import Concur.React.Props (onClick)

-- How many iterations to run at a time
maxIterations :: Int
maxIterations = 10000

tailRecDemo :: forall a. Widget HTML a
tailRecDemo = do
  div'[ p' [text ("Clicking this button will perform " <> show maxIterations <> " iterations via tail recursion ")]
      , p' [text "Once done, you can restart the iterations as many times you want."]
      , do
          button [unit <$ onClick] [text "Start Tail Recursion Demo"]
          tailRecWidget 0 2
      ]

tailRecWidget :: forall a. Int -> Int -> Widget HTML a
tailRecWidget count times = do
  let newCount = count + 1
  if newCount > maxIterations
     then do
       button [unit <$ onClick] [text ("Ran " <> show count <> " iterations. Restart? (n = " <> show times <> ")")]
       tailRecWidget 0 (times + 1)
     else do
       -- For the first maxIterations times, tailRecWidget calls itself in a tight loop
       -- This would blow the stack, if it weren't for the pulse here.
       pulse
       tailRecWidget newCount times
