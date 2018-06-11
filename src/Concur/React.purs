module Concur.React where

import Prelude

import Concur.Core (Widget, mapView)
import Concur.Core.Discharge (discharge, dischargeAsync)
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

-- | Wrap a widget with a node and raw props
el :: forall m a. ShiftMap (Widget HTML) m => NodeName -> m a -> m a
el n = shiftMap (mapView (\v -> [n v]))

-- | Wrap some widgets with a node and raw props
el' :: forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => NodeName -> Array (m a) -> m a
el' n = el n <<< orr

componentClass :: forall props a. Widget HTML a -> R.ReactClass props
componentClass winit = R.createClass (R.spec' init render)
  where
    -- purescript-aff guarantees that sync computations in Aff are resolved synchronously.
    -- That is a good thing in general, but if it happens during init, then setState gets
    --  called before the component has had a chance to mount itself (init is called before mount).
    -- So inside init, we force async widget resolution by using `dischargeAsync` instead of `discharge`.
    --  instead of using setState to set the new view.
    init this = runIOSync' $ dischargeAsync (handler this) winit
    handler this (Right r) = do
      v <- discharge (handler this) r
      void $ liftEff $ R.writeState this v
    handler _ (Left err) = do
      liftEff $ log ("FAILED! " <> show err)
      pure unit
    -- TODO: Refine the div wrapper. This is just a placeholder.
    render this = D.div' <$> R.readState this

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
