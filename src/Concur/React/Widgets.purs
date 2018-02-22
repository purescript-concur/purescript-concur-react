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
-- Or a `Right a` when the inner `Widget HTML eff a` ends.
wrapButton :: forall a eff. Array P.Props -> Widget HTML eff a -> Widget HTML eff (Either Unit a)
wrapButton props w = elEvent (P.onClick <<< (_ <<< const unit)) D.button props w

-- Like a wrapButton, but takes no props
wrapButton' :: forall a eff. Widget HTML eff a -> Widget HTML eff (Either Unit a)
wrapButton' = wrapButton []

-- Specialised button with only static children
displayButton :: forall eff. Array P.Props -> (forall a. Widget HTML eff a) -> Widget HTML eff Unit
displayButton props d = either id id <$> wrapButton props d

-- Like a displayButton, but takes no props
displayButton' :: forall eff. (forall a. Widget HTML eff a) -> Widget HTML eff Unit
displayButton' = displayButton []

-- Specialised text only button
textButton :: forall eff. Array P.Props -> String -> Widget HTML eff Unit
textButton props label = displayButton props (CD.text label)

-- Like a textButton, but takes no props
textButton' :: forall eff. String -> Widget HTML eff Unit
textButton' = textButton []

-- Wrap an element with an arbitrary eventHandler over a widget
elEvent :: forall a b eff. ((a -> EventHandler eff Unit) -> P.Props) -> NodeTag -> Array P.Props -> Widget HTML eff b -> Widget HTML eff (Either a b)
elEvent evt = elEventMany [evt]

-- Wrap an element with multiple arbitrary eventHandlers over a widget
elEventMany :: forall a b eff. (Array ((a -> EventHandler eff Unit) -> P.Props)) -> NodeTag -> Array P.Props -> Widget HTML eff b -> Widget HTML eff (Either a b)
elEventMany evts e props (RenderEnd a) = RenderEnd (Right a)
elEventMany evts e props (Widget w) = Widget $ \send ->
  [e (props <> ((\evt -> evt (send <<< pure <<< Left)) <$> evts)) (w (send <<< map Right))]
elEventMany evts e props (WidgetEff eff) = WidgetEff (elEventMany evts e props <$> eff)

-- Wrap a div with key handlers around a widget
-- Returns a `Left unit on key events.
-- Or a `Right a` when the inner `Widget HTML eff a` ends.
wrapKeyHandler :: forall a eff. Array P.Props -> Widget HTML eff a -> Widget HTML eff (Either R.KeyboardEvent a)
wrapKeyHandler props w = elEvent P.onKeyDown D.div props w

-- Specialised key handler widget with only static children
displayKeyHandler :: forall eff. Array P.Props -> (forall a. Widget HTML eff a) -> Widget HTML eff R.KeyboardEvent
displayKeyHandler props w = either id id <$> wrapKeyHandler props w
