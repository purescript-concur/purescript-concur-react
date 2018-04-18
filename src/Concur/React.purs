module Concur.React where

import Prelude

import Concur.Core (Widget, WidgetStep(..), mapView, orr, unWidget)
import Control.Monad.Aff (runAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Free (resume)
import Control.Monad.IO (runIO')
import Control.Monad.IOSync (runIOSync')
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import React (spec')
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
componentClass winit = R.createClass spec
  where
    spec = (spec' init render) { componentDidMount = componentDidMount }

    init this =
      case resume (unWidget winit) of
        Right _ -> pure { view: [], cont: Nothing }
        Left (WidgetStep mws) -> runIOSync' do
          ws <- mws
          pure { view: ws.view, cont: Just ws.cont }

    componentDidMount this = do
      ws <- R.readState this
      case ws.cont of
        Nothing -> unsafeCrashWith "Nothing to render"
        Just cont -> runAff_ (handler this) $ runIO' cont

    discharge this w = case resume w of
      Right _ -> pure unit
      Left (WidgetStep mws) -> runIOSync' do
        ws <- mws
        void $ liftEff $ R.writeState this { view: ws.view, cont: Nothing }
        liftEff $ runAff_ (handler this) $ runIO' ws.cont

    handler this = case _ of
      Right r -> discharge this r
      Left err -> do
        log ("FAILED! " <> show err)
        pure unit

    -- TODO: Refine the div wrapper. This is just a placeholder.
    render this = do
      { view } <- R.readState this
      pure $ D.div' view

renderComponent :: forall a. Widget HTML a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
