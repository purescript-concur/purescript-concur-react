module Test.Hello where

import Prelude

import Concur.Core (Widget)
import Concur.Core.DevTools (connectDevTools, sendToDevTools)
import Concur.React (HTML)
import Concur.React.DOM (button, text, div')
import Concur.React.Props (onClick)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Tuple (snd)
import Effect.Class (liftEffect)
import Effect.Console (log)

helloWidget :: Widget HTML Int
helloWidget = snd <$> runStateT helloWidgetS 0

helloWidgetS :: forall a. StateT Int (Widget HTML) a
helloWidgetS = do
  conn <- liftEffect connectDevTools
  count <- get
  liftEffect $ sendToDevTools conn "Increment" count
  e <- lift $ div'
    [ but "Say Hello!"
    , but $ "For the " <> show count <> " time, hello sailor!"
    ]
  put (count + 1)
  liftEffect (log "You said Hello!")
  helloWidgetS
  where
    but s = button [onClick] [text s]
