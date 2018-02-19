module Concur.React.Widgets where

import Prelude

import Concur.React (HTML, NodeTag, Widget(..), EventHandler)
import Concur.React.DOM as CD
import Data.Either (Either(..), either)
import React as R
import React.DOM as D
import React.DOM.Props as P

-- Combinator Widgets, at a more higher level than those in Concur.React.DOM

-- Wrap a button around a widget
-- Returns a `Left unit on click events.
-- Or a `Right a` when the inner `Widget HTML a` ends.
wrapButton :: forall a. Array P.Props -> Widget HTML a -> Widget HTML (Either Unit a)
wrapButton props w = elEvent (P.onClick <<< (_ <<< const unit)) D.button props w

-- Like a wrapButton, but takes no props
wrapButton' :: forall a. Widget HTML a -> Widget HTML (Either Unit a)
wrapButton' = wrapButton []

-- Specialised button with only static children
displayButton :: Array P.Props -> (forall a. Widget HTML a) -> Widget HTML Unit
displayButton props d = either id id <$> wrapButton props d

-- Like a displayButton, but takes no props
displayButton' :: (forall a. Widget HTML a) -> Widget HTML Unit
displayButton' = displayButton []

-- Specialised text only button
textButton :: Array P.Props -> String -> Widget HTML Unit
textButton props label = displayButton props (CD.text label)

-- Like a textButton, but takes no props
textButton' :: String -> Widget HTML Unit
textButton' = textButton []

-- Wrap an element with an arbitrary eventHandler over a widget
elEvent :: forall a b. ((a -> EventHandler Unit) -> P.Props) -> NodeTag -> Array P.Props -> Widget HTML b -> Widget HTML (Either a b)
elEvent evt = elEventMany [evt]

-- Wrap an element with multiple arbitrary eventHandlers over a widget
elEventMany :: forall a b. (Array ((a -> EventHandler Unit) -> P.Props)) -> NodeTag -> Array P.Props -> Widget HTML b -> Widget HTML (Either a b)
elEventMany evts e props (RenderEnd a) = RenderEnd (Right a)
elEventMany evts e props (Widget w) = Widget $ \send ->
  [e (props <> ((\evt -> evt (send <<< pure <<< Left)) <$> evts)) (w (send <<< map Right))]

-- Wrap a div with key handlers around a widget
-- Returns a `Left unit on key events.
-- Or a `Right a` when the inner `Widget HTML a` ends.
wrapKeyHandler :: forall a. Array P.Props -> Widget HTML a -> Widget HTML (Either R.KeyboardEvent a)
wrapKeyHandler props w = elEvent P.onKeyDown D.div props w

-- Specialised key handler widget with only static children
displayKeyHandler :: Array P.Props -> (forall a. Widget HTML a) -> Widget HTML R.KeyboardEvent
displayKeyHandler props w = either id id <$> wrapKeyHandler props w
