module Concur.React.Run where

import Concur.Core.Types (Widget)
import Concur.React (HTML, toReactElement)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (<=<), (=<<))
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Function (const, ($), (<<<))
import Data.Functor ((<$>))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, error, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import React (ReactElement)
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

foreign import data Root :: Type

foreign import createRoot :: Element -> Effect Root

foreign import render :: Root -> ReactElement -> Effect Unit

-- | Run a Concur Widget inside a dom element with the specified id
runWidgetInDom :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInDom elemId = runWidgetInSelector ("#" <> elemId)

-- | Run a Concur Widget inside a dom element with the specified selector
runWidgetInSelector :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInSelector elemId = renderWidgetInto (QuerySelector elemId)

-- | Run a Concur Widget inside a dom element with the specified QuerySelector
renderWidgetInto :: forall a. QuerySelector -> Widget HTML a -> Effect Unit
renderWidgetInto query w =
  runAffX do
    awaitLoad
    mroot <- selectElement query
    for mroot \root -> do
      let
        container = HTMLElement.toElement root
      rt <- liftEffect $ createRoot container
      liftEffect $ render rt (toReactElement "Concur" mempty w)

-- ReactDOM.render (toReactElement "Concur" mempty w) container
-- Attribution - Everything below was taken from Halogen.Aff.Utils
-- https://github.com/purescript-halogen/purescript-halogen/blob/master/src/Halogen/Aff/Util.purs
-- | Waits for the document to load.
awaitLoad :: Aff Unit
awaitLoad =
  makeAff \callback -> do
    rs <- readyState =<< Window.document =<< window
    case rs of
      Loading -> do
        et <- Window.toEventTarget <$> window
        listener <- eventListener (\_ -> callback (Right unit))
        addEventListener ET.domcontentloaded listener false et
        pure $ effectCanceler (removeEventListener ET.domcontentloaded listener false et)
      _ -> do
        callback (Right unit)
        pure nonCanceler

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: Aff HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (throwError (error "Could not find body")) pure body

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Aff (Maybe HTMLElement)
selectElement query = do
  mel <-
    liftEffect
      $ ((querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window)
  pure $ HTMLElement.fromElement =<< mel

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runAffX :: forall x. Aff x -> Effect Unit
runAffX = runAff_ (either throwException (const (pure unit)))
