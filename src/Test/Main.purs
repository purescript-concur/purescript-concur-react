module Test.Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Control.Monad.Eff.Console (CONSOLE)

-- Import all examples
import Test.Hello (helloWidget)
import Test.Counter (counterWidget)
import Test.TailRec (tailRecDemo)

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE | eff) Unit
main = do
  runWidgetInDom "hello" helloWidget
  runWidgetInDom "counter" (counterWidget 0 <|> counterWidget 100)
  runWidgetInDom "tailRec" tailRecDemo
