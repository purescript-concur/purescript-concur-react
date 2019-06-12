module Test.TheElmArchitecture where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

import Concur.Core.Dado as Da

-- This is like Elm's State
type Form =
  { name :: String
  , rememberMe :: Boolean
  }

initForm :: Form
initForm = {name: "Anonymous", rememberMe: false}

showForm :: Form -> String
showForm f = "Your name was set to -" <> show f.name <> ". You also requested that we " <> (if f.rememberMe then "" else "don't ") <> "remember you."

-- This is like Elm's Action
data FormAction
  = Name String
  | RememberMe Boolean
  | Submit

formWidget :: Form -> Widget HTML Form
formWidget form = do
  -- This is like Elm's view function
  res <- D.div_ Da.do
    Name <$> D.input [P._type "text", P.value form.name, P.unsafeTargetValue <$> P.onChange]
    RememberMe (not form.rememberMe) <$ D.input [P._type "checkbox", P.checked form.rememberMe, P.onChange]
    Submit <$ do D.button [P.onClick] $ D.text "Submit"
  -- This is like Elm's update function
  case res of
    Name s -> formWidget (form {name = s})
    RememberMe b -> formWidget (form {rememberMe = b})
    Submit -> pure form

teaWidget :: forall a. Widget HTML a
teaWidget = go initForm
  where
  go f = do
    x <- D.div_ Da.do
      D.text (showForm f)
      formWidget f
    go x
