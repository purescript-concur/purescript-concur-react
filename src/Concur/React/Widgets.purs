module Concur.React.Widgets where

import Prelude

import Concur.Core (Widget, loopState)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap, class ShiftUp)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)

-- | A Text input that returns its contents on enter
textInputEnter :: String -> String -> Boolean -> Widget HTML String
textInputEnter value hint reset = do
    e <- D.input [P.defaultValue value, P.onKeyEnter, P.placeholder hint]
    -- HACK: Using forced do notation, to force evaluation of the text input value in the same handler
    new <- pure $ P.unsafeTargetValue e
    when reset $ liftEffect (P.resetTargetValue "" e)
    pure new

-- | A Text input that has a button attached
-- | Returns its contents on the user pressing enter, or clicking the button
textInputWithButton :: forall m. ShiftMap (Widget HTML) m => MultiAlternative m => ShiftUp (Widget HTML) m => Monad m => String -> String -> String -> m String
textInputWithButton value hint buttonlabel = loopState value \s -> D.div'
  [ D.input
    [ Left <<< unsafeGetVal <$> P.onChange
    , Right <<< P.unsafeTargetValue <$> P.onKeyEnter
    , P.defaultValue s
    , P.placeholder hint
    ]
  , D.text " "
  , Right s <$ D.button [P.onClick] [D.text buttonlabel]
  ]
  where
  unsafeGetVal e = (unsafeCoerce e).target.value
