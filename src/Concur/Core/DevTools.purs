module Concur.Core.DevTools where

import Data.Unit (Unit)
import Effect (Effect)

data DevToolsConnection
foreign import connectDevTools :: Effect DevToolsConnection
foreign import disconnectDevTools :: Effect Unit
foreign import sendToDevTools :: forall action state. DevToolsConnection -> action -> state -> Effect Unit
