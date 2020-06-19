module Concur.React.Run where

import Prelude

import Concur.Core.Event (Observer(..), effMap, observe)
import Concur.Core.Types (Widget)
import Concur.React (HTML, renderComponent)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Exception (error)
import ReactDOM as ReactDOM
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

-- | Run a Concur Widget inside a dom element with the specified id
runWidgetInDom :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInDom elemId = runWidgetInSelector ("#" <> elemId)

-- | Run a Concur Widget inside a dom element with the specified selector
runWidgetInSelector :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInSelector elemId = renderWidgetInto (QuerySelector elemId)

-- | Run a Concur Widget inside a dom element with the specified QuerySelector
renderWidgetInto :: forall a. QuerySelector -> Widget HTML a -> Effect Unit
renderWidgetInto query w = runObserver awaitLoad \_ -> do
  mroot <- selectElement query
  case mroot of
    Nothing -> pure unit
    Just root -> void $ ReactDOM.render (renderComponent w) (HTMLElement.toElement root)


-- Attribution - Everything below was taken from Halogen.Aff.Utils
-- https://github.com/purescript-halogen/purescript-halogen/blob/master/src/Halogen/Aff/Util.purs

-- | Waits for the document to load.
awaitLoad :: Observer Unit
awaitLoad = Observer \callback -> do
  rs <- readyState =<< Window.document =<< window
  case rs of
    Loading -> do
      et <- Window.toEventTarget <$> window
      listener <- eventListener (\_ -> callback unit)
      addEventListener ET.domcontentloaded listener false et
      pure $ removeEventListener ET.domcontentloaded listener false et
    _ -> do
      callback unit
      pure (pure unit)

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: Observer HTMLElement
awaitBody = effMap awaitLoad \_ -> do
  body <- selectElement (QuerySelector "body")
  maybe (throwError (error "Could not find body")) pure body

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Effect (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window
  pure $ HTMLElement.fromElement =<< mel

-- | Runs an `Observer` in the background, calling a handler on completion
runObserver :: forall x. Observer x -> (x -> Effect Unit) -> Effect Unit
runObserver o handler = void $ observe o handler
