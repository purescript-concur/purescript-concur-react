module Concur.Stark.Run where

-- import StarkDOM as StarkDOM

-- runWidgetInDom :: forall a. String -> Widget HTML a -> Effect Unit
-- runWidgetInDom elemId w = do
--   win <- DOM.window
--   doc <- DOM.document win
--   let node = DOM.toNonElementParentNode doc
--   mroot <- DOM.getElementById elemId node
--   -- mroot <- getElementById (ElementId elemId) (documentToNonElementParentNode (htmlDocumentToDocument doc))
--   case mroot of
--     Nothing -> pure unit
--     Just root -> void (StarkDOM.render (renderComponent w) root)
