module Stark.Ref
  ( Ref
  , RefHandler
  , StarkInstance
  , NativeNode
  , fromRef
  , fromEffect
  , getCurrentRef
  , createNodeRef
  , createInstanceRef
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

--- | An instance of a Stark class.
data StarkInstance = StarkInstance

--- | A platform-specific native layout node. On the web this will be a DOM
--- | element (see `Web.HTML.HTMLElement`).
data NativeNode = NativeNode
data Ref a = Ref
data RefHandler a = RefHandler


createRef :: forall a. Effect (Ref a)
createRef = pure Ref


liftCallbackRef :: forall a. Ref a -> Ref a
liftCallbackRef r = r


createNodeRef :: Effect (Ref NativeNode)
createNodeRef = createRef


createInstanceRef :: Effect (Ref StarkInstance)
createInstanceRef = createRef


fromRef :: forall a. Ref a -> RefHandler a
fromRef _ = RefHandler


fromEffect :: forall a. (Ref a -> Effect Unit) -> RefHandler a
fromEffect _ = RefHandler


getCurrentRef :: forall a. Ref a -> Effect (Maybe a)
getCurrentRef _ = pure Nothing
