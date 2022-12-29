module Test.FocusCount where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Props (debounce)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

type InputState = {focusCount:: Int, currentText :: String}

initState :: InputState
initState = {focusCount: 0, currentText: ""}

showState :: InputState -> String
showState s = "The current value of the input is " <> show s.currentText <> ",\n and you have focused it " <> show s.focusCount <> " times."

inputWidget :: InputState -> Widget HTML InputState
inputWidget st = do
  D.input [ st {focusCount = st.focusCount+1} <$ P.onFocus
  , ((\s -> st {currentText = s}) <<< P.unsafeTargetValue)  <$> debounce 400 (P.persist P.onChange)
  ]

focusCountWidget :: forall a. Widget HTML a
focusCountWidget = go initState
  where
  go s = D.div'
    [ D.p' [D.text $ showState s]
    , inputWidget s
    ] >>= go
