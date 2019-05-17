module Test.FFI where

import Data.Nullable (Nullable)
import Data.Unit (Unit)
import Effect (Effect)

-- FFI Common to all examples

-- Simple Local storage interface
foreign import storageLength :: Effect Int
foreign import storageGet :: forall a. String -> Effect (Nullable a)
foreign import storageSet :: forall a. String -> a -> Effect Unit
foreign import storageDelete :: String -> Effect Unit
foreign import storageClear :: Effect Unit
