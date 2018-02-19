module Test.Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import DOM (DOM)

-- Import all examples
import Test.Hello (helloWidget)
import Test.Counter (counterWidget)

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  runWidgetInDom "hello" helloWidget
  runWidgetInDom "counter" (counterWidget 0 <|> counterWidget 100)
