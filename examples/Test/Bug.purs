module Test.Bud where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Effect.Class (liftEffect)
import Effect.Console (log)

import Concur.Core.Dado as Da

effect :: Widget HTML Unit
effect = do
  void $ D.button [P.onClick] $ D.text "Button"
  liftEffect $ log "EFFECT"

query :: Widget HTML Boolean
query = go false
  where
    go st = do
      event <- D.div_ Da.do
        effect
        void $ D.button [P.onClick] $ D.text "Query" -- if this goes first everything works
      pure st

bugWidget :: ∀ a. Widget HTML a
bugWidget = do
  a <- query
  D.text (show a)
