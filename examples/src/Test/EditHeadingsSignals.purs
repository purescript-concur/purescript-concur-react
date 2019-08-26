module Test.EditHeadingsSignals where

import Prelude

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, dyn, loopS, step)
import Concur.React (HTML)
import Concur.React.DOM (div', button, h5, text, ul_, li_)
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
  _ <- button [onClick] [text "New"]
  pure (pure (Just (Tree {title: "New Heading", children: []})))

treeView :: Maybe Tree -> Signal HTML (Maybe Tree)
treeView Nothing = pure Nothing
treeView (Just t) = treeView' t
  where
    treeView' (Tree tree) = li_ [] $ do
      title' <- editableTitle tree.title
      shouldDel <- step false (pure true <$ button [onClick] [text "Delete"])
      if shouldDel
        then pure Nothing
        else do
          newChild <- mkChildren
          let children = case newChild of
                Nothing -> tree.children
                Just newt -> cons newt tree.children
          children' <- ul_ [] $ map catMaybes $ traverse treeView' children
          pure (Just (Tree {title: title', children: children'}))

editableTitle :: String -> Signal HTML String
editableTitle title = step title do
  _ <- h5 [onDoubleClick] [text title]
  edited <- div'
    [ textInputEnter title false [placeholder title]
    , title <$ button [onClick] [text "Cancel"]
    ]
  pure $ editableTitle $ if edited == "" then title else edited

editHeadings :: forall a. Widget HTML a
editHeadings = dyn $ ul_ [] $ loopS (Just testTree) treeView
