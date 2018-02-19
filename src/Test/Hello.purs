module Test.Hello where

import Prelude

import Concur.React (Widget, HTML)
import Concur.React.Widgets (textButton')

helloWidget :: forall a. Widget HTML a
helloWidget = do
  textButton' "Click to Say Hello"
  textButton' "Hello Sailor!"
  helloWidget
