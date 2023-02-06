module Test.Keyboard where

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.BooleanAlgebra (not)
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import React.SyntheticEvent as R


-- A virtual keyboard, that also demonstrates how to handle document level events


-- Main Widget ----------------------------------------------------

-- A never-ending virtual keypad widget.
-- Allows the user to navigate and select a key. Displays the selected key.
keypadWidget :: forall a. Widget HTML a
keypadWidget = go Enter "" <|> toggleEvents
  where
  go focus msg = do
    keyPressed <- virtualKeyInput focus <|> D.div' [D.text msg]
    go keyPressed $ "You clicked: " <> show keyPressed

-- On off button for key events
toggleEvents :: forall a. Widget HTML a
toggleEvents = go false
  where
  go enabled = do
    _ <- D.button [P.onClick] [D.text $ if enabled then "stop listening" else "start listening"]
    liftEffect (if enabled then stopListening else startListening)
    go (not enabled)

-- Displays a keypad with the supplied initial focus.
-- Allows the user to navigate and select a key. Returns the selected key.
virtualKeyInput :: Focus -> Widget HTML Key
virtualKeyInput focus = do
  evt <- liftAff awaitKey <|> keypadButtons focus
  key <- liftEffect $ toKey evt
  case key of
    Just Enter -> pure focus
    Nothing -> virtualKeyInput focus
    Just ArrowUp -> virtualKeyInput (transition focus U)
    Just ArrowDown -> virtualKeyInput (transition focus D)
    Just ArrowLeft -> virtualKeyInput (transition focus L)
    Just ArrowRight -> virtualKeyInput (transition focus R)

-- Dispay only. Renders the keypad buttons with the supplied focus
keypadButtons :: forall a. Focus -> Widget HTML a
keypadButtons focus = D.table [spanstyle] $ pure $ D.tbody'
  [ D.tr' [ blank,         but ArrowUp,   blank          ]
  , D.tr' [ but ArrowLeft, but Enter,     but ArrowRight ]
  , D.tr' [ blank,         but ArrowDown, blank          ]
  ]
  where
    blank = D.td' [D.text ""]
    but key = D.td [style key] [D.text (show key)]
    spanstyle = P.style
      { verticalAlign: "middle"
      }
    style key = P.style
      { width: "50px"
      , height: "40px"
      , background: if key==focus then "lightblue" else "gray"
      , textAlign: "center"
      }


-- FFI ------------------------------------------------------------

-- Start and stop listening for keyboard events
foreign import startListening :: Effect Unit
foreign import stopListening :: Effect Unit

-- Await a key input. Requires that we are listening for events.
foreign import _awaitKey :: EffectFnAff R.SyntheticKeyboardEvent
awaitKey :: Aff R.SyntheticKeyboardEvent
awaitKey = fromEffectFnAff _awaitKey


-- Data structures ------------------------------------------------

data Key
  = ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | Enter

instance showKey :: Show Key where
  show ArrowUp = "Up"
  show ArrowDown = "Down"
  show ArrowLeft = "Left"
  show ArrowRight = "Right"
  show Enter = "Enter"

instance eqKey :: Eq Key where
  eq ArrowUp ArrowUp = true
  eq ArrowDown ArrowDown = true
  eq ArrowLeft ArrowLeft = true
  eq ArrowRight ArrowRight = true
  eq Enter Enter = true
  eq _ _ = false

type Focus = Key

data Dir = U | D | L | R

toKey :: R.SyntheticKeyboardEvent -> Effect (Maybe Key)
toKey event = do
  k <- R.key event
  pure $ case k of
    "ArrowUp" -> Just ArrowUp
    "ArrowDown" -> Just ArrowDown
    "ArrowLeft" -> Just ArrowLeft
    "ArrowRight" -> Just ArrowRight
    "Enter" -> Just Enter
    _ -> Nothing

transition :: Key -> Dir -> Key
transition ArrowRight L = Enter
transition Enter L = ArrowLeft
transition ArrowLeft R = Enter
transition Enter R = ArrowRight
transition ArrowUp D = Enter
transition Enter D = ArrowDown
transition ArrowDown U = Enter
transition Enter U = ArrowUp
transition k _ = k
