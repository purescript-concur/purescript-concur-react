module Test.RunOnceEffects where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Trans (StateT, evalStateT)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.AVar as Evar
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as Avar
import Effect.Aff.AVar (AVar)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Routing.Hash (matches)
import Routing.Match (Match, end, int, root)

-- To route, we start listening for route changes with `matches`
-- On each route change we push the route to a var
-- Then we listen on the var asynchronously from within the UI with `awaitRoute`

data Render = None | First | Many

derive instance eqRender :: Eq Render

type MyState a = {
  route :: MyRoute,
  render :: Render,
  cancelers :: AVar (Array (Effect Unit)) | a
}

step :: Render -> Render
step None = First
step First = Many
step Many = Many

routingWidget :: forall a. Widget HTML a
routingWidget = do
  a <- liftAff Avar.empty
  evalStateT routingWidgetSt { route: Home, render: None, cancelers: a }

routingWidgetSt :: forall a b. StateT (MyState b) (Widget HTML) a
routingWidgetSt = do
  st <- get
  routeRef <- liftEffect $ do
    var <- Evar.empty
    void $ matches myRoutes \_ route -> void $ Evar.tryPut route var
    pure var
  -- awaitRoute sets into the state the currently navigated route
  -- then runs and pops any cancelers that are pending
  let awaitRoute = \state -> liftAff $ do
        route <- Avar.take routeRef
        -- run all cancelers
        inner <- Avar.tryTake st.cancelers
        void $ liftEffect $ case inner of
          Just i -> sequence i
          Nothing -> pure []
        void $ Avar.tryPut [] st.cancelers
        pure $ state { route=route }

  -- HACK: This delay is only needed the first time
  -- Since the page might still be loading,
  -- and there are weird interactions between loading the homepage and the current route
  liftAff (delay (Milliseconds 0.0))
  go awaitRoute
  where
  go awaitRoute = do
    s <- get
    state <- pageForRoute <|> awaitRoute s
    void $ modify \st -> st {route=state.route}
    go awaitRoute

-- Route and associated pages
data MyRoute
  = Home
  | Page Int

myRoutes :: Match MyRoute
myRoutes = root *> oneOf
  [ Home <$ end
  , Page <$> int <* end
  ]

instance showRoute :: Show MyRoute where
  show Home = "Home"
  show (Page i) = "Page" <> show i

derive instance eqInstance :: Eq MyRoute

pageForRoute :: forall a b. StateT (MyState b) (Widget HTML) a
pageForRoute = do
  s <- get
  pageForRouteInner s.route

pageForRouteInner :: forall a b. MyRoute -> StateT (MyState b) (Widget HTML) a
pageForRouteInner Home = homePage
pageForRouteInner (Page i) = page i

runOnce ::
  forall a b.
  Effect a ->
  Effect Unit ->
  StateT (MyState b) (Widget HTML) (Maybe a)
runOnce subscriber canceler = do
  s' <- modify \st -> st { render = step st.render }
  case s'.render of
    First -> do
      pushCanceler canceler
      -- run subscriber
      a <- liftEffect subscriber
      pure (Just a)
    _ -> pure Nothing

pushCanceler ::
  forall b.
  Effect Unit ->
  StateT (MyState b) (Widget HTML) Unit
pushCanceler canceler = do
  s <- get
  -- push new canceler
  liftAff $ void $ Avar.tryPut [ canceler ] s.cancelers

homePage :: forall a b. StateT (MyState b) (Widget HTML) a
homePage = do
  st <- get
  void $ runOnce (log "subscribe home") (log "cancel home")
  D.div'[ D.h1' [D.text " You are on the Homepage"]
        , D.div' $ map (\i -> D.div' [D.a [P.href $ "#/" <> show i] [
                                         D.text $ "Page " <> show i]]) [1,2]]

page :: forall a b. Int -> StateT (MyState b) (Widget HTML) a
page i = go i 1
  where
    go i' n = do
      void $ runOnce (log $ "subscribe page " <> show i') (log $ "cancel page " <> show i')
      res <- Just <$> D.div' [
        D.div' [ D.h1' [D.text $ "You are on Page " <> show i'],
                 D.div' [D.a [P.href "#/"] [D.text "Go Home"]]
               ]]  <|>  Nothing <$ D.button [P.onClick] [D.text $ "Render count: " <> (show n)]
      case res of
        Just a -> a
        Nothing -> go i' (n + 1)
