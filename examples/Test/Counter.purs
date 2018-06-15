module Test.Counter where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div', p', text)
import Concur.React.Props (onClick)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , button [onClick] [text "Increment"] $> count+1
        , button [onClick] [text "Decrement"] $> count-1
        ]
  liftEff (log ("COUNT IS NOW: " <> show n))
  counterWidget n
