module Test.Hello where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, text, div')
import Concur.React.Props (onClick)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Tuple (snd)

helloWidget :: Widget HTML Int
helloWidget = snd <$> runStateT helloWidgetS 0

helloWidgetS :: forall a. StateT Int (Widget HTML) a
helloWidgetS = do
  count <- get
  e <- lift $ div'
    [ but "Say Hello!"
    , but $ "For the " <> show count <> " time, hello sailor!"
    ]
  put (count + 1)
  liftEff (log "You said Hello!")
  helloWidgetS
  where
    but s = button [onClick] [text s]
