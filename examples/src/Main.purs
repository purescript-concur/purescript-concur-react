module Main where

import Prelude

import Concur.React.DOM (div_, h2_, hr', text)
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Control.MultiAlternative (orr)
import Effect (Effect)
import Test.Calc (calcWidget)
import Test.Color (colorWidget)
import Test.Counter (counterWidget)
import Test.EditHeadings (editHeadings)
import Test.FeetToInches (feetToInches)
import Test.FocusCount (focusCountWidget)
import Test.Hello (helloWidget)
import Test.JSInterface (renderReactComponent)
import Test.Keyboard (keypadWidget)
import Test.Login (loginWidget)
import Test.Routing (routingWidget)
import Test.Signals (countingWidget)
import Test.SlowButtonList (hugeButtonListDemo)
import Test.TailRec (tailRecDemo)
import Test.TheElmArchitecture (teaWidget)
import Test.Timers (timersWidget)
import Test.Todos (todosWidget)
import Test.Tabs (tabsWidget)

main :: Effect Unit
main = do
    runWidgetInDom "main" $ orr
      [ widget tabsWidget "Tabs"
      , widget renderReactComponent "Bidirectional React JS interop"
      , widget keypadWidget "Virtual Keypad Example"
      , widget helloWidget "Hello World"
      , widget (counterWidget 0 <|> counterWidget 100) "Counter"
      , widget focusCountWidget "Count Focus"
      , widget feetToInches "Bidirectional example widget"
      , widget loginWidget "Login"
      , widget routingWidget "Routing"
      , widget countingWidget "Counting with Signals!"
      , widget editHeadings "Editable Tree"
      , widget todosWidget "Mini Todo List with Signals"
      , widget calcWidget "Postfix Calculator"
      -- , widget ajaxWidget "Ajax Demo"
      , widget (colorWidget "") "Color"
      , widget timersWidget "Timers"
      , widget teaWidget "The Elm Architecture"
      , widget (hugeButtonListDemo 50000) "Huge List of 50 thousand buttons"
      , widget tailRecDemo "Tail Recursion Demo"
      ]
  where
    widget w s = orr
      [ hr'
      , h2_ [] $ text s
      , div_ [] w
      ]
