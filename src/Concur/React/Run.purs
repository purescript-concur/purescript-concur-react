module Concur.React.Run where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML, renderComponent)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (Maybe(..))
import ReactDOM (render)

runWidgetInDom :: forall a eff. String -> Widget HTML a -> Eff (dom :: DOM | eff) Unit
runWidgetInDom elemId w = do
  win <- window
  doc <- document win
  mroot <- getElementById (ElementId elemId) (documentToNonElementParentNode (htmlDocumentToDocument doc))
  case mroot of
    Nothing -> pure unit
    Just root -> void (render (renderComponent w) root)
