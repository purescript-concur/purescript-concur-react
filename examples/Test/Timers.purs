module Test.Timers where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div', h4', text)
import Concur.React.Widgets (textButton')
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Monad.Aff (Milliseconds(..), delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), maybe)

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
          , textButton' "Start timer" >>= \_ -> getNewTime
          ] >>= Just >>> timer
    getNewTime = do
      startTime <- liftEff now
      liftEff $ log $ "Started Timer " <> show idx <> " at time " <> show startTime
      liftAff (delay (Milliseconds 3000.0)) <|> textButton' "Cancel"
      stopTime <- liftEff now
      liftEff $ log $ "Stopped Timer " <> show idx <> " at time " <> show stopTime
      pure $ unInstant stopTime - unInstant startTime
