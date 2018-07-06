module Concur.React.Widgets where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Effect (Effect)
import Effect.Class (liftEffect)
import React.SyntheticEvent (SyntheticKeyboardEvent)
import Unsafe.Coerce (unsafeCoerce)

-- | A Text input that returns its contents on enter
textInputEnter :: String -> Widget HTML String
textInputEnter s = do
    e <- D.input [P.defaultValue s, P.onKeyDown]
    -- Using forced do notation, to force evaluation of the text input value
    new <- pure (unsafeCoerce e).target.value
    if isEnterEvent e
      then do
        liftEffect (resetTargetValue "" e)
        pure new
      else textInputEnter new

-- | Check if a keyboard event was Enter
isEnterEvent :: SyntheticKeyboardEvent -> Boolean
isEnterEvent e =
  e'.which == 13 || e'.keyCode == 13
  where e' = unsafeCoerce e

-- | IMPORTANT: UNSAFE: It's unsafe to use this outside this module
foreign import resetTargetValue :: forall event. String -> event -> Effect Unit
