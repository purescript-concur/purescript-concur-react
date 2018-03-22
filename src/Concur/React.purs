module Concur.React where

import Prelude

import Concur.Core (Widget, WidgetStep(WidgetStep), mapView, orr, unWidget)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (resume)
import Control.Monad.IO (runIO')
import Control.Monad.IOSync (runIOSync')
import Data.Either (Either(..))
import React as R
import React.DOM as D
import React.DOM.Props as P

type HTML = Array R.ReactElement
type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

el :: forall a. NodeName -> Widget HTML a -> Widget HTML a
el n = mapView (\v -> [n v])

el' :: forall a. NodeName -> Array (Widget HTML a) -> Widget HTML a
el' n = el n <<< orr

componentClass :: forall props a. Widget HTML a -> R.ReactClass props
componentClass winit = R.createClass (R.spec' init render)
  where
    init this = discharge (unWidget winit) this
    discharge w this = case resume w of
      Right _ -> pure []
      Left (WidgetStep mws) -> runIOSync' $ do
        ws <- mws
        liftEff $ runAff_ (handler this) $ runIO' ws.cont
        pure ws.view
    handler this (Right r) = do
      v <- discharge r this
      void $ unsafeCoerceEff $ R.writeState this v
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    -- TODO: Refine the div wrapper. This is just a placeholder.
    render this = D.div' <$> R.readState this

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
