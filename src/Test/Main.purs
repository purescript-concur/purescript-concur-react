module Test.Main where

import Prelude

import Concur.React (Render, renderComponent, el, el', text, button)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import React.DOM as D
import ReactDOM (render)
import Control.Alt ((<|>))

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void (getRoot >>= render (renderComponent exampleComponent))

exampleComponent :: forall a. Render a
exampleComponent = counter 0 <|> counter 100

counter :: forall a. Int -> Render a
counter count = do
  n <- el' D.div'
        [ el D.p' (text ("State: " <> show count))
        , (pure (count+1)) <* button "Increment"
        , (pure (count-1)) <* button "Decrement"
        ]
  counter n

getRoot :: forall eff. Eff (dom :: DOM | eff) Element
getRoot = do
  win <- window
  doc <- document win
  elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  pure $ unsafePartial fromJust elm
