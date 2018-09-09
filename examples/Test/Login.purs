module Test.Login where

import Prelude hiding (div)

import Concur.Core (Widget, loopState)
import Concur.React (HTML)
import Concur.React.DOM (button, div, div', input, text)
import Concur.React.Props (onChange, onClick, style)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

type UserLogin = String
type User =
  { name :: String
  , login :: UserLogin
  }

userMap :: Map UserLogin User
userMap = fromFoldable
  [ mkUser "admin" "Admin"
  , mkUser "demo" "demo"
  ]
  where
  mkUser ulogin name = Tuple ulogin {name : name, login: ulogin}

login :: Widget HTML User
login = loopState {msg: "", uname: ""} \s -> div'
  [ div' [text "Try logging in as 'demo' or 'admin'"]
  , div [style { color: "red"}] [text s.msg]
  , div'
    [ input [Left <<< s {uname = _} <<< unsafeGetVal <$> onChange]
    , do
        unit <$ button [onClick] [text "Login"]
        -- The following could be an effectful ajax call, instead of a pure lookup
        pure $ case M.lookup s.uname userMap of
          Nothing -> Left (s {msg = "Login Failed for user '" <> s.uname <> "'"})
          Just u -> Right u
    ]
  ]
  where
  unsafeGetVal e = (unsafeCoerce e).target.value

logout :: User -> Widget HTML Unit
logout u = div'
  [ text ("Logged in as " <> u.login <> " ")
  , unit <$ button [onClick] [text "Logout"]
  ]

loginWidget :: forall a. Widget HTML a
loginWidget = forever do login >>= logout
