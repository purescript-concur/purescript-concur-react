module Test.Hello where

import Prelude

import Concur.Core (Widget)
import Concur.Core.DevTools (StateSubscription, connectDevTools, subscribe, withStateful)
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
helloWidget = do
  conn <- liftEffect connectDevTools
  subs <- liftEffect $ subscribe conn
  snd <$> runStateT (helloWidgetS subs) 0

helloWidgetS :: forall a. StateSubscription Int -> StateT Int (Widget HTML) a
helloWidgetS subs = do
  count <- get
  -- liftEffect $ sendState subs "Increment" count
  newCount <- lift $ withStateful subs "Increment" $ map (const (count + 1)) $ div'
    [ but "Say Hello!"
    , but $ "For the " <> show count <> " time, hello sailor!"
    ]
  put newCount -- (count + 1)
  liftEffect (log "You said Hello!")
  helloWidgetS subs
  where
    but s = button [onClick] [text s]
