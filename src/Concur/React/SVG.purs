module Concur.React.SVG where

import Prelude hiding (div,map,sub)

import Concur.React.DOM (El, El', el')
import React.DOM.SVG as S

-------------------------------------------------------------------------------------------------------------------
circle :: El
circle = el' S.circle

circle' :: El'
circle' = circle []

clipPath :: El
clipPath = el' S.clipPath

clipPath' :: El'
clipPath' = clipPath []

defs :: El
defs = el' S.defs

defs' :: El'
defs' = defs []

ellipse :: El
ellipse = el' S.ellipse

ellipse' :: El'
ellipse' = ellipse []

foreignObject :: El
foreignObject = el' S.foreignObject

g :: El
g = el' S.g

g' :: El'
g' = g []

line :: El
line = el' S.line

line' :: El'
line' = line []

linearGradient :: El
linearGradient = el' S.linearGradient

linearGradient' :: El'
linearGradient' = linearGradient []

mask :: El
mask = el' S.mask

mask' :: El'
mask' = mask []

path :: El
path = el' S.path

path' :: El'
path' = path []

pattern :: El
pattern = el' S.pattern

pattern' :: El'
pattern' = pattern []

polygon :: El
polygon = el' S.polygon

polygon' :: El'
polygon' = polygon []

polyline :: El
polyline = el' S.polyline

polyline' :: El'
polyline' = polyline []

radialGradient :: El
radialGradient = el' S.radialGradient

radialGradient' :: El'
radialGradient' = radialGradient []

rect :: El
rect = el' S.rect

rect' :: El'
rect' = rect []

stop :: El
stop = el' S.stop

stop' :: El'
stop' = stop []

svg :: El
svg = el' S.svg

svg' :: El'
svg' = svg []

text :: El
text = el' S.text

text' :: El'
text' = text []

tspan :: El
tspan = el' S.tspan

tspan' :: El'
tspan' = tspan []
