module Test.Calc where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Patterns (remoteWidget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Control.MultiAlternative (orr)
import Data.List (List(..), uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Possible actions emitted by the Calculator buttons
data CalculatorAction = Plus | Minus | Times | Div | Enter | Clear | Digit Int

-- Button pad widget
calcButtonsWidget :: Widget HTML CalculatorAction
calcButtonsWidget = D.table' $ pure $ D.tbody' $
  [ D.tr' [d 7, d 8, d 9, opDiv]
  , D.tr' [d 4, d 5, d 6, opTimes]
  , D.tr' [d 1, d 2, d 3, opMinus]
  , D.tr' [d 0, ent, cls, opPlus]
  ]
  where
    d n     = but (Digit n) (show n)
    ent     = but Enter "⏎"
    cls     = but Clear "C"
    opDiv   = but Div "/"
    opTimes = but Times "*"
    opMinus = but Minus "-"
    opPlus = but Plus "+"
    but x s = x <$ D.td' [D.button [P.onClick] [D.text s]]

-- Postfix calculation
calc :: List Int -> CalculatorAction -> Tuple (List Int) Int
calc arr axn = case uncons arr, axn of
  Just {head: x, tail: xs}, Digit d -> new (x*10+d) xs
  Nothing                 , Digit d -> new d arr
  _                       , Clear   -> Tuple Nil 0
  _                       , Enter   -> Tuple (0:arr) 0
  Nothing                 , _       -> err
  Just {head: x, tail: xs}, _ -> case uncons xs, axn of
    Just {head: y, tail: ys}, Plus  -> new (x+y) ys
    Just {head: y, tail: ys}, Minus -> new (x-y) ys
    Just {head: y, tail: ys}, Times -> new (x*y) ys
    Just {head: y, tail: ys}, Div   -> new (y `div` x) ys
    _ , _ -> err
  where
    err = Tuple arr 0
    new n s = Tuple (n:s) n

-- We create a display widget that wires everything up
-- It renders the buttons and display in parallel
-- And then handles button input, performs the calculation, and recurses
calcDisplay :: forall a. List Int -> Widget HTML CalculatorAction -> Widget HTML CalculatorAction -> Widget HTML a
calcDisplay st display buttons = do
  a <- display <|> buttons
  let Tuple st' n = calc st a
  calcDisplay st' (D.text (show n)) buttons

-- Using calcDisplay is pretty easy as can be seen in `calcWidgetStandard`
calcWidgetStandard :: forall a. Widget HTML a
calcWidgetStandard = calcDisplay Nil defaultDisplay calcButtonsWidget
  where
  defaultDisplay = D.text "Press a button"

-- But in this example we don't use this "standard" way of doing things

-- Instead, we show off remote widgets to drive the display
-- This code may seem longer, but it's *sometimes* cleaner to use "action at a distance".
-- It's also very useful to avoid having to rework complex logic.

-- For example, what if want the buttons to render in a different place, and not right next to the display?
-- By passing the Buttons widget off to calc, we cede control of where it is rendered
-- But remote widgets can help us recover that control
calcWidget :: forall a. Widget HTML a
calcWidget = do
  -- First create a remote widget for the buttons
  remoteButtons <- remoteWidget calcButtonsWidget
  orr
    -- Then we pass only the yield part of the remote buttons to calcDisplay
    [ calcDisplay Nil defaultDisplay remoteButtons.yield
    -- Meanwhile we render the buttons in a potentially different place
    -- Let's give the buttons a subtle background
    , D.div [ P.style {background: "beige", width: "fit-content"} ] 
        -- Note we don't care WHERE or HOW the events from the buttons are being handled
        -- We only have to forever keep emmitting events
        -- It's a never ending widget that can get information out!
        [ forever remoteButtons.render ]
    ]
  where
  defaultDisplay = D.text "This display is controlled by other widgets. GO AHEAD. PRESS A BUTTON."

