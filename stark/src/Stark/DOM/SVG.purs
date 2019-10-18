module Stark.DOM.SVG where

import Stark (StarkElement)
import Stark.DOM (IsDynamic(..), mkDOM)
import Stark.DOM.Props (Props)

circle :: Array Props -> Array StarkElement -> StarkElement
circle = mkDOM (IsDynamic false) "circle"

circle' :: Array StarkElement -> StarkElement
circle' = circle []

clipPath :: Array Props -> Array StarkElement -> StarkElement
clipPath = mkDOM (IsDynamic false) "clipPath"

clipPath' :: Array StarkElement -> StarkElement
clipPath' = clipPath []

defs :: Array Props -> Array StarkElement -> StarkElement
defs = mkDOM (IsDynamic false) "defs"

defs' :: Array StarkElement -> StarkElement
defs' = defs []

ellipse :: Array Props -> Array StarkElement -> StarkElement
ellipse = mkDOM (IsDynamic false) "ellipse"

ellipse' :: Array StarkElement -> StarkElement
ellipse' = ellipse []

foreignObject :: Array Props -> Array StarkElement -> StarkElement
foreignObject = mkDOM (IsDynamic false) "foreignObject"

g :: Array Props -> Array StarkElement -> StarkElement
g = mkDOM (IsDynamic false) "g"

g' :: Array StarkElement -> StarkElement
g' = g []

line :: Array Props -> Array StarkElement -> StarkElement
line = mkDOM (IsDynamic false) "line"

line' :: Array StarkElement -> StarkElement
line' = line []

linearGradient :: Array Props -> Array StarkElement -> StarkElement
linearGradient = mkDOM (IsDynamic false) "linearGradient"

linearGradient' :: Array StarkElement -> StarkElement
linearGradient' = linearGradient []

mask :: Array Props -> Array StarkElement -> StarkElement
mask = mkDOM (IsDynamic false) "mask"

mask' :: Array StarkElement -> StarkElement
mask' = mask []

path :: Array Props -> Array StarkElement -> StarkElement
path = mkDOM (IsDynamic false) "path"

path' :: Array StarkElement -> StarkElement
path' = path []

pattern :: Array Props -> Array StarkElement -> StarkElement
pattern = mkDOM (IsDynamic false) "pattern"

pattern' :: Array StarkElement -> StarkElement
pattern' = pattern []

polygon :: Array Props -> Array StarkElement -> StarkElement
polygon = mkDOM (IsDynamic false) "polygon"

polygon' :: Array StarkElement -> StarkElement
polygon' = polygon []

polyline :: Array Props -> Array StarkElement -> StarkElement
polyline = mkDOM (IsDynamic false) "polyline"

polyline' :: Array StarkElement -> StarkElement
polyline' = polyline []

radialGradient :: Array Props -> Array StarkElement -> StarkElement
radialGradient = mkDOM (IsDynamic false) "radialGradient"

radialGradient' :: Array StarkElement -> StarkElement
radialGradient' = radialGradient []

rect :: Array Props -> Array StarkElement -> StarkElement
rect = mkDOM (IsDynamic false) "rect"

rect' :: Array StarkElement -> StarkElement
rect' = rect []

stop :: Array Props -> Array StarkElement -> StarkElement
stop = mkDOM (IsDynamic false) "stop"

stop' :: Array StarkElement -> StarkElement
stop' = stop []

svg :: Array Props -> Array StarkElement -> StarkElement
svg = mkDOM (IsDynamic false) "svg"

svg' :: Array StarkElement -> StarkElement
svg' = svg []

text :: Array Props -> Array StarkElement -> StarkElement
text = mkDOM (IsDynamic false) "text"

text' :: Array StarkElement -> StarkElement
text' = text []

tspan :: Array Props -> Array StarkElement -> StarkElement
tspan = mkDOM (IsDynamic false) "tspan"

tspan' :: Array StarkElement -> StarkElement
tspan' = tspan []
