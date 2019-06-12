module Test.FocusCount where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

import Concur.Core.Dado as Da

type InputState = {focusCount:: Int, currentText :: String}

initState :: InputState
initState = {focusCount: 0, currentText: ""}

showState :: InputState -> String
showState s = "The current value of the input is " <> show s.currentText <> ",\n and you have focused it " <> show s.focusCount <> " times."

inputWidget :: InputState -> Widget HTML InputState
inputWidget st = D.input
  [ st {focusCount = st.focusCount+1} <$ P.onFocus
  , ((\s -> st {currentText = s}) <<< P.unsafeTargetValue) <$> P.onChange
  ]

focusCountWidget :: forall a. Widget HTML a
focusCountWidget = go initState
  where
  go s = do
    x <- D.div_ Da.do
      D.p_ $ D.text $ showState s
      inputWidget s
    go x
