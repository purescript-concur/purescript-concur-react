module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Test.Ajax (ajaxWidget)
import Test.Color (colorWidget)
import Test.Counter (counterWidget)
import Test.Hello (helloWidget)
import Test.SlowButtonList (slowButtonListDemo)
import Test.Timers (timersWidget)

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) Unit
main = do
  runWidgetInDom "hello" helloWidget
  runWidgetInDom "counter" (counterWidget 0 <|> counterWidget 100)
  runWidgetInDom "ajax" ajaxWidget
  runWidgetInDom "color" (colorWidget "")
  runWidgetInDom "timers" timersWidget
  runWidgetInDom "slowButtonList" slowButtonListDemo
  -- TODO: This is currently buggy
  -- runWidgetInDom "tailRec" tailRecDemo
