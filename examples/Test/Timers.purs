module Test.Timers where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Dado as Da
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (onClick)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.MultiAlternative (orr)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (negateDuration)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)

timersWidget :: forall a. Widget HTML a
timersWidget = D.div_ $ orr $ map timerWidget [1,2,3,4,5]

timerWidget :: forall a. Int -> Widget HTML a
timerWidget idx = D.div_ Da.do
  D.h4_ $ D.text ("Timer " <> show idx)
  timer Nothing
  where
    timer prevTimeResult = do
      x <- D.div_ Da.do
        maybe empty (\t -> D.div_ $ D.text $ "Previous time - " <> show t) prevTimeResult
        D.button [onClick] (D.text "Start timer") *> getNewTime
      timer (Just x)
    getNewTime = do
      startTime <- liftEffect now
      liftEffect $ log $ "Started Timer " <> show idx <> " at time " <> show startTime
      liftAff (delay (Milliseconds 3000.0)) <|> D.button [unit <$ onClick] (D.text "Cancel")
      stopTime <- liftEffect now
      liftEffect $ log $ "Stopped Timer " <> show idx <> " at time " <> show stopTime
      pure $ unInstant stopTime <> negateDuration (unInstant startTime)
