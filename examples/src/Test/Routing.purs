module Test.Routing where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Routing.Hash (matches)
import Routing.Match (Match, end, int, root)

-- To route, we start listening for route changes with `matches`
-- On each route change we push the route to a var
-- Then we listen on the var asynchronously from within the UI with `awaitRoute`
routingWidget :: forall a. Widget HTML a
routingWidget = do
  routeRef <- liftEffect $ do
    var <- Evar.empty
    void $ matches myRoutes \_ route -> void $ Evar.tryPut route var
    pure var
  let awaitRoute = liftAff $ Avar.take routeRef
  -- HACK: This delay is only needed the first time
  -- Since the page might still be loading,
  -- and there are weird interactions between loading the homepage and the current route
  liftAff (delay (Milliseconds 0.0))
  go awaitRoute Home
  where
  go awaitRoute route = do
    route' <- awaitRoute <|> pageForRoute route
    go awaitRoute route'

-- Route and associated pages
data MyRoute
  = Home
  | Page Int

myRoutes :: Match MyRoute
myRoutes = root *> oneOf
  [ Home <$ end
  , Page <$> int <* end
  ]

pageForRoute :: forall a. MyRoute -> Widget HTML a
pageForRoute Home = homePage
pageForRoute (Page i) = page i

homePage :: forall a. Widget HTML a
homePage = D.div'
  [ D.h1' [D.text " You are on the Homepage"]
  , D.div' $ map (\i -> D.div' [D.a [P.href $ "#/" <> show i] [D.text $ "Page " <> show i]]) [1,2]
  ]

page :: forall a. Int -> Widget HTML a
page i = D.div'
  [ D.h1' [D.text $ "You are on Page " <> show i]
  , D.div' [D.a [P.href "#/"] [D.text "Go Home"]]
  ]
