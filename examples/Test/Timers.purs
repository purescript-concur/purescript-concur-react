module Test.Timers where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div', h4', text)
import Concur.React.Props (onClick)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (negateDuration)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)

timersWidget :: forall a. Widget HTML a
timersWidget = div' (map timerWidget [1,2,3,4,5])

timerWidget :: forall a. Int -> Widget HTML a
timerWidget idx = div'
  [ h4' [text ("Timer " <> show idx)]
  , timer Nothing
  ]
  where
    timer prevTimeResult = do
      div'[ maybe empty (\t -> div' [text ("Previous time - " <> show t)]) prevTimeResult
          , button [onClick] [text "Start timer"] >>= \_ -> getNewTime
          ] >>= Just >>> timer
    getNewTime = do
      startTime <- liftEffect now
      liftEffect $ log $ "Started Timer " <> show idx <> " at time " <> show startTime
      liftAff (delay (Milliseconds 3000.0)) <|> button [unit <$ onClick] [text "Cancel"]
      stopTime <- liftEffect now
      liftEffect $ log $ "Stopped Timer " <> show idx <> " at time " <> show stopTime
      pure $ unInstant stopTime <> negateDuration (unInstant startTime)
