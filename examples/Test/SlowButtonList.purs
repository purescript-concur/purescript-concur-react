module Test.SlowButtonList where

import Prelude

import Concur.Core (Widget, withViewEvent)
import Concur.Core.Dado as Da
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (onClick)
import Control.Alt ((<|>))
import Control.MultiAlternative (orr)
import Data.Array ((..))
import React.DOM as RD
import React.DOM.Props as RP

hugeButtonListDemo :: Int -> Widget HTML Unit
hugeButtonListDemo num = do
  slow <- D.div_ Da.do
    D.text $ "Show a list of " <> show num <> " buttons"
    true <$ do D.button [onClick] $ D.text "SLOW list (may hang the webpage)"
    false <$ do D.button [onClick] $ D.text "FAST list"
  let arr = (1 .. num)
  n <- if slow
     then slowButtonList arr
     else fastButtonList arr
  D.text ("You clicked button#" <> show n) <|> (D.button [unit <$ onClick] $ D.text "Restart?")
  hugeButtonListDemo num

-- Slower but more idiomatic list of buttons
-- Simply use the standard button widget and compose together in a div
slowButtonList :: Array Int -> Widget HTML Int
slowButtonList = D.div_ <<< orr <<< map buttonize
  where buttonize n = D.button [n <$ onClick] $ D.text (show n)

-- Use a lower level interface to create a large number of button views manually
-- This is slightly better than the slow version, because it doesn't create individual aff actions for each button.
fastButtonList :: Array Int -> Widget HTML Int
fastButtonList arr = withViewEvent (\h -> map (\i -> RD.button [RP.onClick (const (h i))] [RD.text (show i)]) arr)
