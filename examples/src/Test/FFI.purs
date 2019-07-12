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

-- Keyboard events. Allows multiple key events at the same time
-- Returns a function which can be called to remove the event listeners
foreign import handleKeyboardEvents :: (Array String -> Effect Unit) -> Effect (Effect Unit)

-- Time Interval events. Calls a handler on each timeout.
-- Returns a function which can be called to remove the event listeners
foreign import setTimeInterval :: Effect Unit -> Effect (Effect Unit)
