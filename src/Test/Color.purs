module Test.Color where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Widgets (textArea, textButton')
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequence)
import React.DOM.Props as P

colorWidget :: forall a eff. String -> Widget HTML a
colorWidget s =
    D.div' [ D.text "Insert some color codes, or "
           , textButton' "get an example" $> exampleText
           , D.div'[textArea [P.style {width: "80%", height: "6em"}] s, showColors s]
           ]
    >>= colorWidget
  where
  matchInput input = either (const Nothing) (flip match input) (regex "#[0-9a-fA-F]{6}" global) >>= sequence
  showColors inp = D.div [P.style { width : "80.0%" }]
                   (maybe [D.text "no colors found"] (map showColor) (matchInput inp))
  showColor col = D.span [P.style (style col)] [D.text col]
  style col = {display:"inline-block", margin:"4px" , textAlign:"center", width:"7em", backgroundColor: col, color: "white"}

exampleText :: String
exampleText = "# from my color scheme\nset $black-alt    #1F2022 set $black        #555F69 set $red-alt      #CB4674 set $red          #FC4474 set $green-alt    #74D55C set $green        #96E931 set $yellow-alt   #A18417 set $yellow       #E89E0F set $blue-alt     #627AD2 set $blue         #4083CD set $magenta-alt  #91429D set $magenta      #D358D5 set $cyan-alt     #25919E set $cyan         #22ABBB set $white-alt    #C1C1C1 set $white        #DFDFDF"
