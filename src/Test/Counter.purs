module Test.Counter where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div', p')
import Concur.React.Widgets (textButton')
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

counterWidget :: forall a. Int -> Widget HTML a
counterWidget count = do
  n <- div'
        [ p' [text ("State: " <> show count)]
        , textButton' "Increment" $> count+1
        , textButton' "Decrement" $> count-1
        ]
  liftEff (log ("COUNT IS NOW: " <> show n))
  counterWidget n
