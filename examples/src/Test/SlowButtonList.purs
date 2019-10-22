module Test.SlowButtonList where

import Prelude

import Concur.Core (Widget, mkLeafWidget)
import Concur.React (HTML)
import Concur.React.DOM (button, div', text)
import Concur.React.Props (onClick)
import Control.Alt ((<|>))
import Data.Array ((..))
import React.DOM as D
import React.DOM.Props as P

hugeButtonListDemo :: Int -> Widget HTML Unit
hugeButtonListDemo num = do
  slow <- div'
    [ text $ "Show a list of " <> show num <> " buttons"
    , true <$ button [onClick] [text "SLOW list (may hang the webpage)"]
    , false <$ button [onClick] [text "FAST list"]
    ]
  let arr = (1 .. num)
  n <- if slow
     then slowButtonList arr
     else fastButtonList arr
  text ("You clicked button#" <> show n) <|> button [unit <$ onClick] [text "Restart?"]
  hugeButtonListDemo num

-- Slower but more idiomatic list of buttons
-- Simply use the standard button widget and compose together in a div
slowButtonList :: Array Int -> Widget HTML Int
slowButtonList = div' <<< map buttonize
  where buttonize n = button [n <$ onClick] [text (show n)]

-- Use a lower level interface to create a large number of button views manually
-- This is slightly better than the slow version, because it doesn't create individual aff actions for each button.
fastButtonList :: Array Int -> Widget HTML Int
fastButtonList arr = mkLeafWidget (\h -> map (\i -> D.button [P.onClick (const (h i))] [D.text (show i)]) arr)
