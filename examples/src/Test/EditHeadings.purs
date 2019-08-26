module Test.EditHeadings where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div', h5, h5_, li_, text, ul_)
import Concur.React.Props (onClick, onDoubleClick, placeholder)
import Concur.React.Widgets (textInputEnter)
import Control.MultiAlternative (orr)
import Data.Array (cons)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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

mkChildren :: Widget HTML Tree
mkChildren = do
  _ <- button [onClick] [text "New"]
  pure $ Tree {title: "New Heading", children: []}

data Action
  = EditTitle String
  | Delete
  | AddChild Tree
  | EditChildren (Array Tree)

displayList :: forall a. (a -> Widget HTML (Maybe a)) -> Array a -> Widget HTML (Array a)
displayList render = go
  where
  go elems = do
    Tuple i res <- orr childElements
    pure $ case res of
      Nothing -> updateElems (A.deleteAt i elems)
      Just updatedElem -> updateElems (A.updateAt i updatedElem elems)
    where
    childElements = elems # A.mapWithIndex \i a -> (Tuple i <$> render a)
    updateElems Nothing = elems
    updateElems (Just elems') = elems'

treeView :: Tree -> Widget HTML (Maybe Tree)
treeView (Tree tree) = li_ [] $ do
  res <- orr
    [ EditTitle <$> editableTitle tree.title
    , Delete <$ button [onClick] [text "Delete"]
    , AddChild <$> mkChildren
    , EditChildren <$> (ul_ [] $ displayList treeView tree.children)
    ]
  case res of
    EditTitle newTitle -> ret {title: newTitle, children: tree.children}
    Delete -> pure Nothing
    AddChild newChild -> ret {title: tree.title, children: cons newChild tree.children}
    EditChildren newChildren -> ret {title: tree.title, children: newChildren}
  where
    ret t = pure (Just (Tree t))

editableTitle :: String -> Widget HTML String
editableTitle title = do
  _ <- h5 [onDoubleClick] [text title]
  edited <- div'
    [ textInputEnter title false [placeholder title]
    , title <$ button [onClick] [text "Cancel"]
    ]
  if edited == "" || edited == "title" then editableTitle title else pure edited

editHeadings :: forall a. Widget HTML a
editHeadings = go testTree
  where
    go tree = do
      newTree <- ul_ [] $ treeView tree
      case newTree of
        Nothing -> h5_ [] $ text "Tree Deleted"
        Just tree' -> go tree'
