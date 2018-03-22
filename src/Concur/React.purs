module Concur.React where

import Prelude

import Concur.Core (Widget(Widget), WidgetStep(WidgetStep), mapView, orr)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
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
componentClass init = R.createClass (R.spec init render)
  where
    handler this (Right r) = void (R.writeState this (Widget r))
    handler _ (Left err) = do
      log ("FAILED! " <> show err)
      pure unit
    render this = do
      Widget w <- R.readState this
      case resume w of
        Right _ -> pure (D.div' [])
        Left (WidgetStep mws) -> runIOSync' $ do
          ws <- mws
          liftEff $ runAff_ (handler this) $ runIO' ws.cont
          -- TODO: Refine the div wrapper. This is just a placeholder.
          pure (D.div' ws.view)

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
