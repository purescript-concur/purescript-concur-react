module Test.Color where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Dado as Da
import Concur.Core.FRP (Signal, dyn, hold, step)
import Concur.React (HTML)
import Concur.React.DOM (button, text, textarea)
import Concur.React.DOM as D
import Concur.React.Props (onChange, onClick, style, value)
import Control.Alternative (empty)
import Control.MultiAlternative (orr)
import Data.Array.NonEmpty (toArray)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequence)
import Unsafe.Coerce (unsafeCoerce)

colorSignal :: String -> Signal HTML String
colorSignal s = step s do
  s' <- D.div_ Da.do
    D.text "Insert some color codes, or "
    button [onClick] (text "get an example") $> exampleText
    D.div_ $ textarea [value s, targetValue <$> onChange, style {width: "80%", height: "6em"}] empty
  pure (colorSignal s')
  where
    -- This seems ridiculous
    targetValue e = (unsafeCoerce e).target.value

showColors :: String -> Signal HTML String
showColors inp = hold inp do
  D.div [style { width : "80.0%" }]
    (maybe (D.text "no colors found") (orr <<< map showColor <<< toArray) (matchInput inp))
  where
  matchInput input = either (const Nothing) (flip match input) (regex "#[0-9a-fA-F]{6}" global) >>= sequence
  showColor col = D.span [style (colstyle col)] $ D.text col
  colstyle col = {display:"inline-block", margin:"4px" , textAlign:"center", width:"7em", backgroundColor: col, color: "white"}

colorWidget :: forall a. String -> Widget HTML a
colorWidget s = dyn do
  s' <- colorSignal s
  showColors s'

exampleText :: String
exampleText = "# from my color scheme\nset $black-alt    #1F2022 set $black        #555F69 set $red-alt      #CB4674 set $red          #FC4474 set $green-alt    #74D55C set $green        #96E931 set $yellow-alt   #A18417 set $yellow       #E89E0F set $blue-alt     #627AD2 set $blue         #4083CD set $magenta-alt  #91429D set $magenta      #D358D5 set $cyan-alt     #25919E set $cyan         #22ABBB set $white-alt    #C1C1C1 set $white        #DFDFDF"
