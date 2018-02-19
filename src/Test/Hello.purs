module Test.Hello where

import Prelude

import Concur.React (HTML, Widget)
import Concur.React.Widgets (textButton')
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)

helloWidget :: forall a eff. Widget HTML (console :: CONSOLE | eff) a
helloWidget = do
  textButton' "Click to Say Hello"
  liftEff (log "You said Hello!")
  textButton' "Hello Sailor!"
  helloWidget
