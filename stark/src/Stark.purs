module Stark
  ( TagName
  , StarkElement(..)
  ) where

import Data.Foldable (fold)
import Data.Functor (map)
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Stark.DOM.Props (Props(..))

type TagName = String
data StarkElement
  = StarkNode TagName (Array Props) (Array StarkElement)
  | StarkText String
  | StarkInt Int
  | StarkNumber Number

instance showStarkElement :: Show StarkElement where
  show (StarkNode tname props children) =
    "<" <> tname <> " " <>  fold (map show props) <> ">" <> fold (map show children) <> "</" <> tname <> ">"
  show (StarkText t) = show t
  show (StarkInt i) = show i
  show (StarkNumber n) = show n
