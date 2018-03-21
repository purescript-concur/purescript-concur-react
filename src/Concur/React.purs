module Concur.React where

import Prelude

import Concur.Core (Widget(Widget), WidgetStep(WidgetStep), mapView, orr)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (resume)
import Control.Monad.IO (launchIO)
import Control.Monad.IOSync (runIOSync')
import Data.Either (Either(..))
import React as R
import React.DOM as D
import React.DOM.Props as P

type HTML = Array R.ReactElement
type NodeName = Array R.ReactElement -> R.ReactElement
type NodeTag = Array P.Props -> Array R.ReactElement -> R.ReactElement

el :: forall a eff. NodeName -> Widget HTML a -> Widget HTML a
el n = mapView (\v -> [n v])

el' :: forall a eff. NodeName -> Array (Widget HTML a) -> Widget HTML a
el' n = el n <<< orr

componentClass :: forall props a. Widget HTML a -> R.ReactClass props
componentClass init = R.createClass (R.spec init render)
  where
    render this = do
      Widget w <- R.readState this
      case resume w of
        Right _ -> pure (D.div' [])
        Left (WidgetStep mws) -> runIOSync' $ do
          ws <- mws
          launchIO (ws.cont >>= (liftEff <<< R.writeState this <<< Widget))
          -- TODO: Refine the div wrapper. This is just a placeholder.
          pure (D.div' ws.view)

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
