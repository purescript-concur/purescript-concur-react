module Test.EditHeadings where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Dado as Da
import Concur.Core.FRP (Signal, dyn, loopS, step)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (onClick, onDoubleClick, placeholder)
import Concur.React.Widgets (textInputEnter)
import Data.Array (catMaybes, cons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

newtype Tree = Tree
  { title :: String
  , children :: Array Tree
  }

testTree :: Tree
testTree = Tree
  { title: "Double click to edit me"
  , children:
    [ Tree
      { title: "Or use the \"delete\" button to delete me"
      , children: []
      }
    , Tree
      { title: "Or use the \"new\" button to add a sub node"
      , children: []
      }
    ]
  }

mkChildren :: Signal HTML (Maybe Tree)
mkChildren = step Nothing $ do
  _ <- D.button [onClick] $ D.text "New"
  pure (pure (Just (Tree {title: "New Heading", children: []})))

treeView :: Maybe Tree -> Signal HTML (Maybe Tree)
treeView Nothing = pure Nothing
treeView (Just t) = treeView' t
  where
    treeView' (Tree tree) = D.li [] $ do
      title' <- editableTitle tree.title
      shouldDel <- step false (pure true <$ D.button [onClick] (D.text "Delete"))
      if shouldDel
        then pure Nothing
        else do
          newChild <- mkChildren
          let children = case newChild of
                Nothing -> tree.children
                Just newt -> cons newt tree.children
          children' <- D.ul [] $ map catMaybes $ traverse treeView' children
          pure (Just (Tree {title: title', children: children'}))

editableTitle :: String -> Signal HTML String
editableTitle title = step title do
  _ <- D.h5 [onDoubleClick] $ D.text title
  edited <- D.div_ Da.do
    textInputEnter title false [placeholder title]
    title <$ D.button [onClick] (D.text "Cancel")
  pure $ editableTitle $ if edited == "" then title else edited

editHeadings :: forall a. Widget HTML a
editHeadings = dyn $ D.ul [] $ loopS (Just testTree) treeView
