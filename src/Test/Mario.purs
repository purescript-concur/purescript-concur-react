module Test.Mario where

import Prelude

import Concur.React (Widget, HTML)
import Concur.React.DOM (text, div', p')
import Concur.React.Widgets (textButton')

marioWidget :: forall a eff. Widget HTML eff a
marioWidget = counter 0

counter :: forall a eff. Int -> Widget HTML eff a
counter count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , textButton' "Increment" $> count+1
        , textButton' "Decrement" $> count-1
        ]
  counter n
