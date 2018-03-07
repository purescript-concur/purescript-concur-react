module Test.Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX)

-- Import all examples
import Test.Hello (helloWidget)
import Test.Counter (counterWidget)
import Test.Ajax (ajaxWidget)
-- import Test.TailRec (tailRecDemo)

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) Unit
main = do
  runWidgetInDom "hello" helloWidget
  runWidgetInDom "counter" (counterWidget 0 <|> counterWidget 100)
  runWidgetInDom "ajax" ajaxWidget
  -- TODO: This is currently buggy
  -- runWidgetInDom "tailRec" tailRecDemo
