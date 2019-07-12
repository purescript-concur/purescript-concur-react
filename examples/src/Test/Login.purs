module Test.Login where

import Prelude hiding (div)

import Concur.Core (Widget)
import Concur.Core.Patterns (loopState)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Widgets (textInputWithButton)
import Control.Monad.State (StateT, evalStateT, get)
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

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

------------------------------------------------------------

login :: Widget HTML User
login = loopState {msg: "", uname: ""} \s -> do
  uname <- D.div'
    [ D.div' [D.text "Try logging in as 'demo' or 'admin'"]
    , D.div [P.style { color: "red"}] [D.text s.msg]
    , D.div' [ textInputWithButton s.uname "Login" [P.placeholder "Enter Username"] [] ]
    ]
  -- The following could be an effectful ajax call, instead of a pure lookup
  pure $ case M.lookup uname userMap of
    Nothing -> Left (s {msg = "Login Failed for user '" <> uname <> "'"})
    Just u -> Right u

logout :: User -> Widget HTML Unit
logout u = D.div'
  [ D.text ("Logged in as " <> u.login <> " ")
  , unit <$ D.button [P.onClick] [D.text "Logout"]
  ]

-- PAGES ---------------------------------------------------

centerPage :: forall a. String -> Widget HTML a -> Widget HTML a
centerPage title contents = D.div'
  [ D.h1' [D.text title]
  , D.div'[ contents ]
  ]

loggedInPage :: forall a. String -> User -> Widget HTML a -> Widget HTML (Maybe a)
loggedInPage title user contents = D.div'
  [ D.h1' [D.text title]
  , D.div'[ Nothing <$ logout user ]
  , D.div'[ Just <$> contents ]
  ]

------------------------------------------------------------

type St = User
type Task v a = StateT St (Widget v) a

currentUser :: forall v. Task v User
currentUser = get

runTask :: forall a. Task HTML a -> Widget HTML a
runTask task = do
  u <- centerPage "Login" login
  ma <- loggedInPage "Task" u (evalStateT task u)
  maybe (runTask task) pure ma

------------------------------------------------------------

loginWidget :: forall a. Widget HTML a
loginWidget = runTask do
  u <- currentUser
  D.div'
    [ D.text "HELLO!"
    ]
