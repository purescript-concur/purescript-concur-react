module Test.Counter where

import Prelude

import Concur.React (Widget, HTML)
import Concur.React.DOM (text, div', p')
import Concur.React.Widgets (textButton')

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , textButton' "Increment" $> count+1
        , textButton' "Decrement" $> count-1
        ]
  counterWidget n
