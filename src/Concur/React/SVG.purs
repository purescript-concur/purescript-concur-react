module Concur.React.SVG where

import Prelude hiding (div,map,sub)

import Concur.React.DOM (El, El_, el)
import React.DOM.SVG as S

-------------------------------------------------------------------------------------------------------------------
circle :: El
circle = el S.circle

circle_ :: El_
circle_ = circle []

clipPath :: El
clipPath = el S.clipPath

clipPath_ :: El_
clipPath_ = clipPath []

defs :: El
defs = el S.defs

defs_ :: El_
defs_ = defs []

ellipse :: El
ellipse = el S.ellipse

ellipse_ :: El_
ellipse_ = ellipse []

foreignObject :: El
foreignObject = el S.foreignObject

g :: El
g = el S.g

g_ :: El_
g_ = g []

line :: El
line = el S.line

line_ :: El_
line_ = line []

linearGradient :: El
linearGradient = el S.linearGradient

linearGradient_ :: El_
linearGradient_ = linearGradient []

mask :: El
mask = el S.mask

mask_ :: El_
mask_ = mask []

path :: El
path = el S.path

path_ :: El_
path_ = path []

pattern :: El
pattern = el S.pattern

pattern_ :: El_
pattern_ = pattern []

polygon :: El
polygon = el S.polygon

polygon_ :: El_
polygon_ = polygon []

polyline :: El
polyline = el S.polyline

polyline_ :: El_
polyline_ = polyline []

radialGradient :: El
radialGradient = el S.radialGradient

radialGradient_ :: El_
radialGradient_ = radialGradient []

rect :: El
rect = el S.rect

rect_ :: El_
rect_ = rect []

stop :: El
stop = el S.stop

stop_ :: El_
stop_ = stop []

svg :: El
svg = el S.svg

svg_ :: El_
svg_ = svg []

text :: El
text = el S.text

text_ :: El_
text_ = text []

tspan :: El
tspan = el S.tspan

tspan_ :: El_
tspan_ = tspan []
