module Test.Color where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (dyn, hold)
import Concur.Core.FRP as S
import Concur.React (HTML)
import Concur.React.DOM (button, text, textarea)
import Concur.React.DOM as D
import Concur.React.Props (onChange, onClick, style, value)
import Data.Array.NonEmpty (toArray)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (sequence)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.State (StateT, get, put, evalStateT)

type Signal a = S.SignalT (StateT String (Widget HTML)) a

colorSignal :: Signal Unit
colorSignal = hold unit do
  s <- get
  s' <- D.div'
    [ D.text "Insert some color codes, or "
    , button [onClick] [text "get an example"] $> exampleText
    , D.div' [textarea [value s, targetValue <$> onChange, style {width: "80%", height: "6em"}] []]
    ]
  put s'
  where
    -- This seems ridiculous
    targetValue e = (unsafeCoerce e).target.value

showColors :: Signal Unit
showColors = hold unit do
  inp <- get
  out <- D.div [style { width : "80.0%" }]
    (maybe [D.text "no colors found"] (map showColor <<< toArray) (matchInput inp))
  put out
  where
  matchInput input = either (const Nothing) (flip match input) (regex "#[0-9a-fA-F]{6}" global) >>= sequence
  showColor col = D.span [style (colstyle col)] [D.text col]
  colstyle col = {display:"inline-block", margin:"4px" , textAlign:"center", width:"7em", backgroundColor: col, color: "white"}

colorWidget :: forall a. String -> Widget HTML a
colorWidget s = flip evalStateT s $ dyn do
  colorSignal
  showColors

exampleText :: String
exampleText = "# from my color scheme\nset $black-alt    #1F2022 set $black        #555F69 set $red-alt      #CB4674 set $red          #FC4474 set $green-alt    #74D55C set $green        #96E931 set $yellow-alt   #A18417 set $yellow       #E89E0F set $blue-alt     #627AD2 set $blue         #4083CD set $magenta-alt  #91429D set $magenta      #D358D5 set $cyan-alt     #25919E set $cyan         #22ABBB set $white-alt    #C1C1C1 set $white        #DFDFDF"
