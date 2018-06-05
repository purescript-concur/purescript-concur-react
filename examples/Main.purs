module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Test.Ajax (ajaxWidget)
import Test.Calc (calcWidget)
import Test.Color (colorWidget)
import Test.Counter (counterWidget)
import Test.Hello (helloWidget)
import Test.Signals (countingWidget)
import Test.SlowButtonList (hugeButtonListDemo)
import Test.TailRec (tailRecDemo)
import Test.Timers (timersWidget)

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) Unit
main = do
  runWidgetInDom "hello" helloWidget
  runWidgetInDom "counter" (counterWidget 0 <|> counterWidget 100)
  runWidgetInDom "signals" countingWidget
  runWidgetInDom "calc" calcWidget
  runWidgetInDom "ajax" ajaxWidget
  runWidgetInDom "color" (colorWidget "")
  runWidgetInDom "timers" timersWidget
  runWidgetInDom "hugeButtonList" (hugeButtonListDemo 50000)
  runWidgetInDom "tailRecursionDemo" tailRecDemo
