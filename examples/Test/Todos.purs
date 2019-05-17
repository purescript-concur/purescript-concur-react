module Test.Todos where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, always, dyn, loopS, loopW, step)
import Concur.Core.Patterns (retryUntil)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Widgets (textInputEnter)
import Control.Lazy (defer)
import Data.Array (catMaybes, cons)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Traversable (traverse)

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

todosWidget :: forall a. Widget HTML a
todosWidget = dyn $ todos {filter: All, todos: []}

mkTodo :: Array Todo -> Signal HTML (Array Todo)
mkTodo ts = loopW ts \ts' -> D.div' $ pure do
  s <- retryUntil (not <<< null) $ textInputEnter "" true [P.placeholder "What do you want to do?"]
  pure (cons {name: s, done: false} ts')

todos :: Todos -> Signal HTML Todos
todos s = loopS s \s' -> do
  ts <- mkTodo s'.todos
  ts' <- map catMaybes (traverse (todo s'.filter) ts)
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
