module Test.Todos where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, always, hold, loopS, loopW, oneShot)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Widgets (textInputEnter)
import Control.Lazy (defer)
import Data.Array (catMaybes, cons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

-- A proof of concept, Mini TodoMVC with Signals!
-- Supports Todo creation, editing, and deletion.

todosWidget :: forall a. Widget HTML a
todosWidget = do
  ts <- oneShot $ todos []
  D.div'[ D.div' [D.text ("Your list: " <> show ts)]
        , todosWidget
        ]

mkTodo :: Array String -> Signal HTML (Array String)
mkTodo ts = loopW ts \ts' -> flip cons ts' <$> textInputEnter ""

todos :: Array String -> Signal HTML (Maybe (Array String))
todos p = do
  ts <- loopS p \p' -> mkTodo p' >>= (map catMaybes <<< traverse todo)
  hold Nothing $ (pure (Just ts)) <$ D.button [P.onClick] [D.text "Persist"]

todo :: String -> Signal HTML (Maybe String)
todo t = hold (Just t) $ D.div'
    [ do _ <- D.span [P.onDoubleClick] [D.text t]
         todo <$> D.span' [textInputEnter t]
    , defer (\_ -> always Nothing) <$ D.button [P.onClick] [D.text "Delete"]
    ]
