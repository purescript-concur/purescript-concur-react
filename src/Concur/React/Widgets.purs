module Concur.React.Widgets where

import Prelude

import Concur.Core (Widget(Widget), WidgetStep(WidgetStep))
import Concur.React (HTML, NodeTag)
import Concur.React.DOM as CD
import Control.Monad.Aff.AVar (takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.AVar (makeEmptyVar, tryPutVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (liftF, resume, wrap)
import Control.Monad.IOSync (IOSync, runIOSync')
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (alt)
import Data.Either (Either(..), either)
import React as R
import React.DOM as D
import React.DOM.Props as P

-- Combinator Widgets, at a more higher level than those in Concur.React.DOM

-- Wrap a button around a widget
-- Returns a `Left unit on click events.
-- Or a `Right a` when the inner `Widget HTML a` ends.
wrapButton :: forall a. Array P.Props -> Widget HTML a -> Widget HTML (Either Unit a)
wrapButton props w = elEvent (\h -> P.onClick (const (runIOSync' (h unit)))) D.button props w

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

-- Helper
withViewEvent :: forall a. ((a -> IOSync Unit) -> HTML) -> Widget HTML a
withViewEvent mkView = Widget (liftF (WidgetStep (do
     v <- liftEff makeEmptyVar
     pure { view: mkView (\a -> void (liftEff (tryPutVar a v))), cont: liftAff (takeVar v) }
  )))

textArea :: Array P.Props -> String -> Widget HTML String
textArea props contents = withViewEvent (\h -> [D.textarea (props <> [P.value contents, P.onChange (runIOSync' <<< h <<< getEventTargetValueString)]) []])

textArea' :: String -> Widget HTML String
textArea' = textArea []

textInput :: Array P.Props -> String -> Widget HTML String
textInput props contents = withViewEvent (\h -> [D.input (props <> [P._type "text", P.value contents, P.onChange (runIOSync' <<< h <<< getEventTargetValueString)]) []])

textInput' :: String -> Widget HTML String
textInput' = textInput []

-- Wrap an element with an arbitrary eventHandler over a widget
elEvent :: forall a b. ((a -> IOSync Unit) -> P.Props) -> NodeTag -> Array P.Props -> Widget HTML b -> Widget HTML (Either a b)
elEvent evt = elEventMany [evt]

-- Wrap an element with multiple arbitrary eventHandlers over a widget
elEventMany :: forall a b. (Array ((a -> IOSync Unit) -> P.Props)) -> NodeTag -> Array P.Props -> Widget HTML b -> Widget HTML (Either a b)
elEventMany evts e props (Widget w) = Widget $
  case resume w of
    Right a -> pure (Right a)
    Left (WidgetStep wsm) -> wrap $ WidgetStep $ do
      ws <- wsm
      var <- liftEff makeEmptyVar
      let view' = [e (props <> ((\evt -> evt (\a -> void (liftEff (tryPutVar (pure (Left a)) var)))) <$> evts)) ws.view]
      let cont' = sequential (alt (parallel (liftAff (takeVar var))) (parallel (map (map Right) ws.cont)))
      pure {view: view', cont: cont'}

-- Wrap a div with key handlers around a widget
-- Returns a `Left unit on key events.
-- Or a `Right a` when the inner `Widget HTML a` ends.
wrapKeyHandler :: forall a. Array P.Props -> Widget HTML a -> Widget HTML (Either R.KeyboardEvent a)
wrapKeyHandler props w = elEvent (\h -> P.onKeyDown (runIOSync' <<< h)) D.div props w

-- Specialised key handler widget with only static children
displayKeyHandler :: Array P.Props -> (forall a. Widget HTML a) -> Widget HTML R.KeyboardEvent
displayKeyHandler props w = either id id <$> wrapKeyHandler props w

-- Generic function to get info out of events
-- TODO: Move these to some other place
foreign import getEventTargetValueString :: R.Event -> String
