module Test.PreEffect where

import Prelude

import Concur.Core (Widget, effAction, pulse, unsafeBlockingEffAction)
import Concur.React (HTML)
import Concur.React.DOM (button, div', text)
import Concur.React.Props (onClick)
import Effect.Class (liftEffect)
import Effect.Console (log)

preEffectWidget :: forall a. Widget HTML a
preEffectWidget = div' [example 1, text "asd", example 2, text "HELLO", example 3, example 4]

example :: forall a. Int -> Widget HTML a
example x = do
  -- pulse
  liftEffect (log $ "example" <> show x)
  _ <- button [onClick] [text $ "example" <> show x]
  example x

example' :: forall a. Int -> Widget HTML a
example' x = do
  _ <- button [onClick] [text $ "example" <> show x]
  liftEffect (log $ "example" <> show x)
  example' x
