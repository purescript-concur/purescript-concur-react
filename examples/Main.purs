module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Effect (Effect)
import Test.Ajax (ajaxWidget)
import Test.Calc (calcWidget)
import Test.Color (colorWidget)
import Test.Counter (counterWidget)
import Test.EditHeadings (editHeadings)
import Test.Login (loginWidget)
import Test.PreEffect (preEffectWidget)
import Test.Signals (countingWidget)
import Test.SlowButtonList (hugeButtonListDemo)
import Test.TailRec (tailRecDemo)
import Test.Timers (timersWidget)
import Test.Todos (todosWidget)

main :: Effect Unit
main = do
  runWidgetInDom "todos" todosWidget
  runWidgetInDom "editHeadings" editHeadings
  runWidgetInDom "hello" preEffectWidget
  -- runWidgetInDom "counter" (counterWidget 0 <|> counterWidget 100)
  -- runWidgetInDom "signals" countingWidget
  -- runWidgetInDom "calc" calcWidget
  -- runWidgetInDom "ajax" ajaxWidget
  -- runWidgetInDom "color" (colorWidget "")
  runWidgetInDom "timers" timersWidget
  -- runWidgetInDom "hugeButtonList" (hugeButtonListDemo 50000)
  -- runWidgetInDom "tailRecursionDemo" tailRecDemo
  -- runWidgetInDom "login" loginWidget
