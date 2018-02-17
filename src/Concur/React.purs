module Concur.React where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Plus (class Alt, alt, class Plus, empty)
import Control.Alternative (class Alternative, (<|>))
import React as R
import React.DOM as D
import React.DOM.Props as P
import Data.Array (foldl)

type ReactEff state refs =
  ( state :: R.ReactState state
  , props :: R.ReactProps
  , refs  :: R.ReactRefs refs
  )

type View = Array R.ReactElement

type RenderFunc a = (Render a -> EventHandler Unit) -> View
data Render a = RenderEnd a | Render (RenderFunc a)

instance renderFunctor :: Functor Render where
  map f (RenderEnd a) = RenderEnd (f a)
  map f (Render r) = Render $ \h -> r (h <<< map f)

instance renderBind :: Bind Render where
  bind (RenderEnd a) k = k a
  bind (Render r) k = Render $ \h -> r (h <<< flip bind k)

instance renderApplicative :: Applicative Render where
  pure = RenderEnd

instance renderApply :: Apply Render where
  apply = ap

instance renderMonad :: Monad Render

instance renderAlt :: Alt Render where
  alt (RenderEnd a) _ = RenderEnd a
  alt _ (RenderEnd a) = RenderEnd a
  alt (Render f) (Render g) = Render $ \h ->
    let hf (RenderEnd a) = h (RenderEnd a)
        hf ff = h (alt ff (Render g))
        hg (RenderEnd b) = h (RenderEnd b)
        hg gg = h (alt (Render f) gg)
    in f hf <> g hg

instance renderPlus :: Plus Render where
  empty = display []

instance renderAlternative :: Alternative Render

mapView :: forall a. (View -> View) -> Render a -> Render a
mapView f (Render r) = Render $ f <<< r
mapView _ r = r

type NodeName = Array R.ReactElement -> R.ReactElement

never :: forall a. Render a
never = empty

button :: String -> Render Unit
button label = renderFunc $ \send -> [D.button [P.onClick \_ -> send (pure unit)] [D.text label]]

text :: forall a. String -> Render a
text label = renderFunc $ \_ -> [ D.text label ]

orr :: forall a. Array (Render a) -> Render a
orr = foldl (<|>) never

el :: forall a. NodeName -> Render a -> Render a
el n = mapView (\v -> [n v])

el' :: forall a. NodeName -> Array (Render a) -> Render a
el' n = el n <<< orr

renderFunc :: forall a. RenderFunc a -> Render a
renderFunc f = Render f

display :: forall a. View -> Render a
display = renderFunc <<< const

type EventHandler a = Eff (ReactEff R.ReadWrite R.ReadOnly) a
type RenderHandler a = Eff (ReactEff R.ReadOnly ()) a
type This props a = R.ReactThis props (Render a)

componentClass :: forall props a. Render a -> R.ReactClass props
componentClass init = R.createClass (R.spec init render)
  where
    render :: This props a -> RenderHandler R.ReactElement
    render this = do
      v <- renderInner this
      -- TODO: REFINE THE DIV WRAPPER. THIS IS JUST A PLACEHOLDER
      pure (D.div' v)
    renderInner :: This props a -> RenderHandler View
    renderInner this = do
      ren <- R.readState this
      case ren of
        RenderEnd a -> pure []
        Render renderView -> do
          let handler r' = void (R.writeState this r')
          pure (renderView handler)

renderComponent :: forall a. Render a -> R.ReactElement
renderComponent init = R.createFactory (componentClass init) {}
