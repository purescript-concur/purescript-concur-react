module Test.Todos where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, always, dyn, loopS, loopW, fireOnce_, step)
import Concur.Core.Patterns (retryUntil)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Widgets (textInputEnter)
import Control.Lazy (defer)
import Data.Array (catMaybes, cons, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.String (Pattern(..), codePointFromChar, countPrefix, null, split)
import Data.String.CodePoints (drop, take)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Test.FFI (storageGet, storageSet)

-- A proof of concept, Mini TodoMVC with Signals!
-- Supports Todo creation, completion, editing, and deletion.

data Filter = All | Active | Completed
derive instance eqFilter :: Eq Filter
instance showFilter :: Show Filter where
  show All = "All"
  show Active = "Active"
  show Completed = "Completed"

type Todo = {name :: String, done :: Boolean}
type Todos = {filter :: Filter, todos :: Array Todo}

serialiseTodos :: Array Todo -> String
serialiseTodos todosArr = intercalate "\n" (map serialiseTodo todosArr)
  where serialiseTodo {name, done} = name <> "\t" <> if done then "T" else "F"

deserialiseTodos :: String -> Array Todo
deserialiseTodos s = if null s then [] else
  let deserialiseTodo t =
        let prefixLen = countPrefix (_ /= (codePointFromChar '\t')) t
        in { name: take prefixLen t, done: drop (prefixLen+1) t /= "F" }
  in map deserialiseTodo (split (Pattern "\n") s)

localStorageKey :: String
localStorageKey = "todos"

todosWidget :: forall a. Widget HTML a
todosWidget = do
  savedTodosNullable <- liftEffect $ storageGet localStorageKey
  let savedTodos = fromMaybe [] $ map deserialiseTodos $ toMaybe savedTodosNullable
  dyn $ todos {filter: All, todos: savedTodos}

mkTodo :: Array Todo -> Signal HTML (Array Todo)
mkTodo ts = loopW ts \ts' -> D.div' $ pure do
  s <- retryUntil (not <<< null) $ textInputEnter "" true [P.placeholder "What do you want to do?"]
  let newTodos = cons {name: s, done: false} ts'
  pure newTodos

todos :: Todos -> Signal HTML Todos
todos s = loopS s \s' -> do
  ts <- mkTodo s'.todos
  ts' <- map catMaybes (traverse (todo s'.filter) ts)
  fireOnce_ $ liftEffect $ storageSet localStorageKey (serialiseTodos ts')
  filterButtons s' {todos = ts'}

todo :: Filter -> Todo -> Signal HTML (Maybe Todo)
todo p t = if runFilter p t
  then step (Just t) $ D.div'
    [ todo p <<< (\b -> t {done = b}) <$> checkbox t.done
    , do _ <- D.span [mark t.done, P.onDoubleClick] [D.text t.name]
         todo p <<< (\s -> t {name = s}) <$> D.span' [retryUntil (not <<< null) $ textInputEnter t.name false []]
    , defer (\_ -> always Nothing) <$ D.button [P.onClick] [D.text "Delete"]
    ]
  else always (Just t)
  where
    runFilter All _ = true
    runFilter Active t' = not t'.done
    runFilter Completed t' = t'.done
    checkbox b = not b <$ D.input [P._type "checkbox", P.checked b, P.onChange]
    mark done = if done
      then P.style {textDecoration: "line-through"}
      else P.style {}

filterButtons :: Todos -> Signal HTML Todos
filterButtons s = step s $ D.div' (mkFilter <$> filters)
  where
    mkFilter f = D.button [select f, defer (\_ -> filterButtons (s {filter = f})) <$ P.onClick] [D.text (show f)]
    filters = [All, Active, Completed]
    select f = if s.filter == f
      then P.style {border:"2px solid lightgray"}
      else P.style {}
