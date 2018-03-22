module Test.Mario where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div', p')
import Concur.React.Widgets (textButton')

marioWidget :: forall a. Widget HTML a
marioWidget = counter 0

counter :: forall a. Int -> Widget HTML a
counter count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , textButton' "Increment" $> count+1
        , textButton' "Decrement" $> count-1
        ]
  counter n
