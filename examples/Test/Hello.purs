module Test.Hello where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DomProps (input)
import Concur.React.Props (_type, defaultValue, onClick)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

helloWidget :: forall a. Widget HTML a
helloWidget = do
  button "Click to Say Hello"
  liftEff (log "You said Hello!")
  button "Hello Sailor!"
  helloWidget
  where
    button s = input [_type "button", defaultValue s, unit <$ onClick] []
