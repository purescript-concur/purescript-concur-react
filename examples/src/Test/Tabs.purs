module Test.Tabs where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Patterns (internalise)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..), either)

-- The tabs component is used to demonstrate 'Concur.Core.Patterns.internalise'
-- internalise allows us to "hide" the state of a widget, such that
-- the resulting widget can be scheduled multiple times without losing state

-- A simple stateful widget
counterWidget :: Int -> Widget HTML Int
counterWidget count = D.div'
  [ D.p' [D.text ("State: " <> show count)]
  , D.button [P.onClick] [D.text "Increment"] $> count+1
  , D.button [P.onClick] [D.text "Decrement"] $> count-1
  ]


-- A dead simple tabs widget that takes children widgets as an argument
-- Whenever a child returns, the entire tab component returns
tabs :: forall a. Array (Widget HTML a) -> Widget HTML a
tabs children = go 0
  where
  go sel = do
    v <- D.div [] [ Left <$> renderTabs sel children, Right <$> renderPanels sel children ]
    either go pure v

  renderTabs :: Int -> Array (Widget HTML a) -> Widget HTML Int
  renderTabs selected = D.div [] <<< Array.mapWithIndex \i _ ->
    D.button [i <$ P.onClick, color i] [D.text (show i)]
    where
      color i = P.style {background: if i == selected then "white" else "beige"}
  
  renderPanels :: Int -> Array (Widget HTML a) -> Widget HTML a
  renderPanels selected = D.div [] <<< Array.mapWithIndex \i w -> do
    D.div [] [if i == selected then w else mempty]

-- Now we want to show a stateful widget (counter widget) as one of the tabs
-- But the problem is that the counter will reset as soon as any of the children
-- of the tab return a value. The standard Concur solution for this would be
-- to return a sum type (Either CounterState ChildState) from the tab
-- and then route the state update appropriately to either the counter or other children
-- However, sometimes that's too cumbersome, or requires rewriting too much logic.
-- In such cases we can use "internalise"
-- Try running the tabsWidget below and you will see that the counter maintains state!
tabsWidget :: forall a. Widget HTML a
tabsWidget = do
  -- First internalise the stateful widget
  -- Now counter is a never ending widget that maintains its state
  counter <- internalise counterWidget 0
  go counter (-1)
  where
  go counter selected =
    let 
      msg = D.div [] [D.text ("You have selected " <> show selected)]
      -- We include counter in the list of children
      -- Note that there is no need to handle counter events, or manage counter state
      ui = tabs $ map styleIt $ map child (1..4) <> [counter]
    in (msg <|> ui) >>= go counter
  child selected = D.button [selected <$ P.onClick] [ D.text ("Child " <> show selected) ]
  styleIt w = D.div [P.style {height: "100px", width: "100px", background: "#eee"}] [ w ]

