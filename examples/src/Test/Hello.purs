module Test.Hello where

import Prelude hiding (div)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div', text)
import Concur.React.Run (runWidgetInDom)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)

type WidgetT a
  = ReaderT String (Widget a)

textWidget :: forall a. WidgetT HTML a
textWidget = do
  message <- ask
  div' [ text message ]

helloWidget :: forall a. Widget HTML a
helloWidget = runReaderT textWidget "hello"
