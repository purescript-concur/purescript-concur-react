module Test.SlowButtonList where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div', text)
import Concur.React.Widgets (textButton')
import Control.Alt ((<|>))
import Data.Array ((..))

slowButtonListDemo :: Widget HTML Unit
slowButtonListDemo = do
  textButton' "Show a HUGE list of 5000 buttons"
  n <- slowButtonList (1 .. 5000)
  text ("You clicked button#" <> show n) <|> textButton' "Restart?"
  slowButtonListDemo


slowButtonList :: Array Int -> Widget HTML Int
slowButtonList = div' <<< map buttonize
  where
    buttonize n = textButton' (show n) $> n
