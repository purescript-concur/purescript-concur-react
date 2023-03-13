module Test.Hello where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML, toReactClass)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT, runStateT)
import React (ReactClass)

helloWidget :: forall a. Widget HTML a
helloWidget = do
  void $ runStateT helloWidgetS 0
  D.text "Actually this will never be reached, but the compiler is not smart enough to deduce that"

helloWidgetS :: forall a. StateT Int (Widget HTML) a
helloWidgetS = forever do
  count <- get
  void $ D.div' [ D.button [P.onClick] [D.text ("For the " <> show count <> " time, hello sailor!")] ]
  put (count + 1)

-- This widget class is imported directly from a JS component. See JSInterface.js
helloWidgetClass :: ReactClass {}
helloWidgetClass = toReactClass "HelloWidget" mempty (const helloWidget)
