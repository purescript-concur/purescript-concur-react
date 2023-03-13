module Test.JSInterface where

import Concur.Core.Types (Widget, display)
import Concur.React (HTML)
import Concur.React.DOM as D
import React (ReactClass)
import React as React

-- This module demonstrates how to call a Concur Widget inside a JS React component, and vice versa

-- We will embed a simple concur widget from another module (to avoid circular dependencies) inside a JS React component
-- (Test.Hello.helloWidgetClass)

-- A JS react component that we will access from inside a Concur widget
foreign import reactComponent :: ReactClass {}

-- Convert the JS react component to a concur widget
renderReactComponent :: forall a. Widget HTML a
renderReactComponent = D.div'
  [ display [React.createLeafElement reactComponent {}]
  , D.h4' [D.text "Which is all running inside a top level Concur Widget"]
  ]
