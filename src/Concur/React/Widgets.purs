module Concur.React.Widgets where

import Prelude

import Concur.Core (Widget(..), WidgetStep(..), mapView, orr)
import Concur.React (EventHandler, HTML, NodeTag, NodeName)
import Concur.React.DOM as CD
import Control.Monad.Eff.AVar (AVar, makeEmptyVar, takeVar, tryPutVar)
import Control.Monad.Free (liftF)
import Control.Plus (class Plus)
import Control.ShiftMap (class ShiftMap, shiftMap)
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

textArea :: forall eff. Array P.Props -> String -> Widget HTML eff String
textArea props contents = Widget $ \send ->
  viewStep [D.textarea (props <> [P.value contents, P.onChange (send <<< pure <<< getEventTargetValueString)]) []]

textArea' :: forall eff. String -> Widget HTML eff String
textArea' = textArea []

textInput :: forall eff. Array P.Props -> String -> Widget HTML eff String
textInput props contents = Widget $ \send ->
  viewStep [D.input (props <> [P._type "text", P.value contents, P.onChange (send <<< pure <<< getEventTargetValueString)]) []]

textInput' :: forall eff. String -> Widget HTML eff String
textInput' = textInput []

-- Wrap an element with an arbitrary eventHandler over a widget
elEvent :: forall a b eff. ((a -> EventHandler eff Unit) -> P.Props) -> NodeTag -> Array P.Props -> Widget HTML eff b -> Widget HTML eff (Either a b)
elEvent evt = elEventMany [evt]

-- Wrap an element with multiple arbitrary eventHandlers over a widget
elEventMany :: forall a b eff. (Array ((a -> EventHandler eff Unit) -> P.Props)) -> NodeTag -> Array P.Props -> Widget HTML eff b -> Widget HTML eff (Either a b)
elEventMany evts e props (RenderEnd a) = RenderEnd (Right a)
elEventMany evts e props (Widget w) = Widget $ \send ->
  forViewStep (w (send <<< map Right)) (\v -> [e (props <> ((\evt -> evt (send <<< pure <<< Left)) <$> evts)) v])
-- elEventMany evts e props (WidgetEff eff) = WidgetEff (elEventMany evts e props <$> eff)

-- Wrap a div with key handlers around a widget
-- Returns a `Left unit on key events.
-- Or a `Right a` when the inner `Widget HTML eff a` ends.
wrapKeyHandler :: forall a eff. Array P.Props -> Widget HTML eff a -> Widget HTML eff (Either R.KeyboardEvent a)
wrapKeyHandler props w = elEvent P.onKeyDown D.div props w

-- Specialised key handler widget with only static children
displayKeyHandler :: forall eff. Array P.Props -> (forall a. Widget HTML eff a) -> Widget HTML eff R.KeyboardEvent
displayKeyHandler props w = either id id <$> wrapKeyHandler props w

-- Generic function to get info out of events
-- TODO: Move these to some other place
foreign import getEventTargetValueString :: R.Event -> String
