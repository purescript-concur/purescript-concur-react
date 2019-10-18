module Concur.Stark where

import Stark (StarkElement)

type HTML
  = Array StarkElement

-- renderComponent :: forall a. Widget HTML a -> R.StarkElement
-- renderComponent init =
