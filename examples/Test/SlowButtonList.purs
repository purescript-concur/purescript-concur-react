module Test.SlowButtonList where

import Prelude

import Concur.Core (Widget, withViewEvent)
import Concur.React (HTML)
import Concur.React.DOM (div', text)
import Concur.React.Widgets (textButton')
import Control.Alt ((<|>))
import Control.Monad.IOSync (runIOSync')
import Data.Array ((..))
import React.DOM as D
import React.DOM.Props as P

hugeButtonListDemo :: Int -> Widget HTML Unit
hugeButtonListDemo num = do
  slow <- div'
    [ text $ "Show a list of " <> show num <> " buttons"
    , true <$ textButton' "SLOW list (may hang the webpage)"
    , false <$ textButton' "FAST list"
    ]
  let arr = (1 .. num)
  n <- if slow
     then slowButtonList arr
     else fastButtonList arr
  text ("You clicked button#" <> show n) <|> textButton' "Restart?"
  hugeButtonListDemo num

-- Slower but more idiomatic list of buttons
-- Simply use the standard button widget and compose together in a div
slowButtonList :: Array Int -> Widget HTML Int
slowButtonList = div' <<< map buttonize
  where buttonize n = textButton' (show n) $> n

-- Use a lower level interface to create a large number of button views manually
-- This is slightly better than the slow version, because it doesn't create individual aff actions for each button.
fastButtonList :: Array Int -> Widget HTML Int
fastButtonList arr = withViewEvent (\h -> map (\i -> D.button [P.onClick (const (runIOSync' (h i)))] [D.text (show i)]) arr)
