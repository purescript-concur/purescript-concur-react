module Stark
  ( TagName
  , StarkElement(..)
  ) where

import Stark.DOM.Props (Props)

type TagName = String
data StarkElement
  = StarkNode TagName (Array Props) (Array StarkElement)
  | StarkText String
  | StarkInt Int
  | StarkNumber Number
