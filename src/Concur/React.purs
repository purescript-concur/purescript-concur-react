module Concur.React where

import Prelude

import Concur.Core (Widget, discharge, mapView)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.IOSync (runIOSync')
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Either (Either(..))
import React as R
import React.DOM as D
import React.DOM.Props as P

type HTML = Array R.ReactElement
type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

el :: forall m a. ShiftMap (Widget HTML) m => NodeName -> m a -> m a
el n = shiftMap (mapView (\v -> [n v]))

el' :: forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => NodeName -> Array (m a) -> m a
el' n = el n <<< orr

componentClass :: forall props a. Widget HTML a -> R.ReactClass props
componentClass winit = R.createClass (R.spec' init render)
  where
    init this = runIOSync' $ discharge winit (handler this)
    handler this (Right r) = do
      v <- discharge r (handler this)
      void $ liftEff $ R.writeState this v
    handler _ (Left err) = do
      liftEff $ log ("FAILED! " <> show err)
      pure unit
    -- TODO: Refine the div wrapper. This is just a placeholder.
    render this = D.div' <$> R.readState this

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
