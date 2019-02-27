module Concur.React.Run where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML, renderComponent)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

import ReactDOM as ReactDOM

runWidgetInDom :: forall a. String -> Widget HTML a -> Effect Unit
runWidgetInDom elemId w = do
  win <- DOM.window
  doc <- DOM.document win
  let node = DOM.toNonElementParentNode doc
  mroot <- DOM.getElementById elemId node
  -- mroot <- getElementById (ElementId elemId) (documentToNonElementParentNode (htmlDocumentToDocument doc))
  case mroot of
    Nothing -> pure unit
    Just root -> void (ReactDOM.render (renderComponent w) root)
