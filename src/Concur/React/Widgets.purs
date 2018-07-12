module Concur.React.Widgets where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Effect.Class (liftEffect)

-- | A Text input that returns its contents on enter
textInputEnter :: String -> String -> Boolean -> Widget HTML String
textInputEnter s p reset = do
    e <- D.input [P.defaultValue s, P.onKeyEnter, P.placeholder p]
    -- HACK: Using forced do notation, to force evaluation of the text input value in the same handler
    new <- pure $ P.unsafeTargetValue e
    when reset $ liftEffect (P.resetTargetValue "" e)
    pure new
