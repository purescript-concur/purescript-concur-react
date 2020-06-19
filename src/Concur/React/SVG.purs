module Concur.React.SVG where

import Prelude hiding (div,map,sub)

import Concur.React.DOM (El, El', el', viewAdapter)
import React.DOM.SVG as S

-------------------------------------------------------------------------------------------------------------------
circle :: El
circle = el' $ viewAdapter S.circle

circle' :: El'
circle' = circle []

clipPath :: El
clipPath = el' $ viewAdapter S.clipPath

clipPath' :: El'
clipPath' = clipPath []

defs :: El
defs = el' $ viewAdapter S.defs

defs' :: El'
defs' = defs []

ellipse :: El
ellipse = el' $ viewAdapter S.ellipse

ellipse' :: El'
ellipse' = ellipse []

foreignObject :: El
foreignObject = el' $ viewAdapter S.foreignObject

g :: El
g = el' $ viewAdapter S.g

g' :: El'
g' = g []

line :: El
line = el' $ viewAdapter S.line

line' :: El'
line' = line []

linearGradient :: El
linearGradient = el' $ viewAdapter S.linearGradient

linearGradient' :: El'
linearGradient' = linearGradient []

mask :: El
mask = el' $ viewAdapter S.mask

mask' :: El'
mask' = mask []

path :: El
path = el' $ viewAdapter S.path

path' :: El'
path' = path []

pattern :: El
pattern = el' $ viewAdapter S.pattern

pattern' :: El'
pattern' = pattern []

polygon :: El
polygon = el' $ viewAdapter S.polygon

polygon' :: El'
polygon' = polygon []

polyline :: El
polyline = el' $ viewAdapter S.polyline

polyline' :: El'
polyline' = polyline []

radialGradient :: El
radialGradient = el' $ viewAdapter S.radialGradient

radialGradient' :: El'
radialGradient' = radialGradient []

rect :: El
rect = el' $ viewAdapter S.rect

rect' :: El'
rect' = rect []

stop :: El
stop = el' $ viewAdapter S.stop

stop' :: El'
stop' = stop []

svg :: El
svg = el' $ viewAdapter S.svg

svg' :: El'
svg' = svg []

text :: El
text = el' $ viewAdapter S.text

text' :: El'
text' = text []

tspan :: El
tspan = el' $ viewAdapter S.tspan

tspan' :: El'
tspan' = tspan []
