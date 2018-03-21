module Test.Hello where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Widgets (textButton')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

helloWidget :: forall a. Widget HTML a
helloWidget = do
  textButton' "Click to Say Hello"
  liftEff (log "You said Hello!")
  textButton' "Hello Sailor!"
  helloWidget
