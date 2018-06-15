module Concur.React.DomProps where

import Concur.Core (display, Widget)
import Concur.React (HTML)
import Concur.React.Props (Props)
import Concur.React.Widgets (elProps')
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Prelude (pure, (<<<))
import React.DOM as D

-- elProp wrappers for all DOM elements from purescript-react
-- BIG TODO: Use ShiftMap
-- TODO: Generate these mechanically somehow

text :: forall a. String -> Widget HTML a
text = display <<< pure <<< D.text

type El = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (Props a) -> Array (m a) -> m a
type El' = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (m a) -> m a

a :: El
a = elProps' D.a

a' :: El'
a' = a []

abbr :: El
abbr = elProps' D.abbr

abbr' :: El'
abbr' = abbr []

address :: El
address = elProps' D.address

address' :: El'
address' = address []

area :: El
area = elProps' D.area

area' :: El'
area' = area []

article :: El
article = elProps' D.article

article' :: El'
article' = article []

aside :: El
aside = elProps' D.aside

aside' :: El'
aside' = aside []

audio :: El
audio = elProps' D.audio

audio' :: El'
audio' = audio []

b :: El
b = elProps' D.b

b' :: El'
b' = b []

base :: El
base = elProps' D.base

base' :: El'
base' = base []

bdi :: El
bdi = elProps' D.bdi

bdi' :: El'
bdi' = bdi []

bdo :: El
bdo = elProps' D.bdo

bdo' :: El'
bdo' = bdo []

big :: El
big = elProps' D.big

big' :: El'
big' = big []

blockquote :: El
blockquote = elProps' D.blockquote

blockquote' :: El'
blockquote' = blockquote []

body :: El
body = elProps' D.body

body' :: El'
body' = body []

br :: El
br = elProps' D.br

br' :: El'
br' = br []

button :: El
button = elProps' D.button

button' :: El'
button' = button []

canvas :: El
canvas = elProps' D.canvas

canvas' :: El'
canvas' = canvas []

caption :: El
caption = elProps' D.caption

caption' :: El'
caption' = caption []

cite :: El
cite = elProps' D.cite

cite' :: El'
cite' = cite []

code :: El
code = elProps' D.code

code' :: El'
code' = code []

col :: El
col = elProps' D.col

col' :: El'
col' = col []

colgroup :: El
colgroup = elProps' D.colgroup

colgroup' :: El'
colgroup' = colgroup []

_data :: El
_data = elProps' D._data

_data' :: El'
_data' = _data []

datalist :: El
datalist = elProps' D.datalist

datalist' :: El'
datalist' = datalist []

dd :: El
dd = elProps' D.dd

dd' :: El'
dd' = dd []

del :: El
del = elProps' D.del

del' :: El'
del' = del []

details :: El
details = elProps' D.details

details' :: El'
details' = details []

dfn :: El
dfn = elProps' D.dfn

dfn' :: El'
dfn' = dfn []

dialog :: El
dialog = elProps' D.dialog

dialog' :: El'
dialog' = dialog []

div :: El
div = elProps' D.div

div' :: El'
div' = div []

dl :: El
dl = elProps' D.dl

dl' :: El'
dl' = dl []

dt :: El
dt = elProps' D.dt

dt' :: El'
dt' = dt []

em :: El
em = elProps' D.em

em' :: El'
em' = em []

embed :: El
embed = elProps' D.embed

embed' :: El'
embed' = embed []

fieldset :: El
fieldset = elProps' D.fieldset

fieldset' :: El'
fieldset' = fieldset []

figcaption :: El
figcaption = elProps' D.figcaption

figcaption' :: El'
figcaption' = figcaption []

figure :: El
figure = elProps' D.figure

figure' :: El'
figure' = figure []

footer :: El
footer = elProps' D.footer

footer' :: El'
footer' = footer []

form :: El
form = elProps' D.form

form' :: El'
form' = form []

h1 :: El
h1 = elProps' D.h1

h1' :: El'
h1' = h1 []

h2 :: El
h2 = elProps' D.h2

h2' :: El'
h2' = h2 []

h3 :: El
h3 = elProps' D.h3

h3' :: El'
h3' = h3 []

h4 :: El
h4 = elProps' D.h4

h4' :: El'
h4' = h4 []

h5 :: El
h5 = elProps' D.h5

h5' :: El'
h5' = h5 []

h6 :: El
h6 = elProps' D.h6

h6' :: El'
h6' = h6 []

head :: El
head = elProps' D.head

head' :: El'
head' = head []

header :: El
header = elProps' D.header

header' :: El'
header' = header []

hr :: El
hr = elProps' D.hr

hr' :: El'
hr' = hr []

html :: El
html = elProps' D.html

html' :: El'
html' = html []

i :: El
i = elProps' D.i

i' :: El'
i' = i []

iframe :: El
iframe = elProps' D.iframe

iframe' :: El'
iframe' = iframe []

img :: El
img = elProps' D.img

img' :: El'
img' = img []

input :: El
input = elProps' D.input

input' :: El'
input' = input []

ins :: El
ins = elProps' D.ins

ins' :: El'
ins' = ins []

kbd :: El
kbd = elProps' D.kbd

kbd' :: El'
kbd' = kbd []

keygen :: El
keygen = elProps' D.keygen

keygen' :: El'
keygen' = keygen []

label :: El
label = elProps' D.label

label' :: El'
label' = label []

legend :: El
legend = elProps' D.legend

legend' :: El'
legend' = legend []

li :: El
li = elProps' D.li

li' :: El'
li' = li []

link :: El
link = elProps' D.link

link' :: El'
link' = body []

main :: El
main = elProps' D.main

main' :: El'
main' = main []

map :: El
map = elProps' D.map

map' :: El'
map' = map []

mark :: El
mark = elProps' D.mark

mark' :: El'
mark' = mark []

menu :: El
menu = elProps' D.menu

menu' :: El'
menu' = menu []

menuitem :: El
menuitem = elProps' D.menuitem

menuitem' :: El'
menuitem' = menuitem []

meta :: El
meta = elProps' D.meta

meta' :: El'
meta' = meta []

meter :: El
meter = elProps' D.meter

meter' :: El'
meter' = meter []

nav :: El
nav = elProps' D.nav

nav' :: El'
nav' = nav []

noscript :: El
noscript = elProps' D.noscript

noscript' :: El'
noscript' = noscript []

object :: El
object = elProps' D.object

object' :: El'
object' = object []

ol :: El
ol = elProps' D.ol

ol' :: El'
ol' = ol []

optgroup :: El
optgroup = elProps' D.optgroup

optgroup' :: El'
optgroup' = optgroup []

option :: El
option = elProps' D.option

option' :: El'
option' = option []

output :: El
output = elProps' D.output

output' :: El'
output' = output []

p :: El
p = elProps' D.p

p' :: El'
p' = p []

param :: El
param = elProps' D.param

param' :: El'
param' = param []

picture :: El
picture = elProps' D.picture

picture' :: El'
picture' = picture []

pre :: El
pre = elProps' D.pre

pre' :: El'
pre' = pre []

progress :: El
progress = elProps' D.progress

progress' :: El'
progress' = progress []

q :: El
q = elProps' D.q

q' :: El'
q' = q []

rp :: El
rp = elProps' D.rp

rp' :: El'
rp' = rp []

rt :: El
rt = elProps' D.rt

rt' :: El'
rt' = rt []

ruby :: El
ruby = elProps' D.ruby

ruby' :: El'
ruby' = ruby []

s :: El
s = elProps' D.s

s' :: El'
s' = s []

samp :: El
samp = elProps' D.samp

samp' :: El'
samp' = samp []

script :: El
script = elProps' D.script

script' :: El'
script' = script []

section :: El
section = elProps' D.section

section' :: El'
section' = section []

select :: El
select = elProps' D.select

select' :: El'
select' = select []

small :: El
small = elProps' D.small

small' :: El'
small' = small []

source :: El
source = elProps' D.source

source' :: El'
source' = source []

span :: El
span = elProps' D.span

span' :: El'
span' = span []

strong :: El
strong = elProps' D.strong

strong' :: El'
strong' = strong []

style :: El
style = elProps' D.style

style' :: El'
style' = style []

sub :: El
sub = elProps' D.sub

sub' :: El'
sub' = sub []

summary :: El
summary = elProps' D.summary

summary' :: El'
summary' = summary []

sup :: El
sup = elProps' D.sup

sup' :: El'
sup' = sup []

table :: El
table = elProps' D.table

table' :: El'
table' = table []

tbody :: El
tbody = elProps' D.tbody

tbody' :: El'
tbody' = tbody []

td :: El
td = elProps' D.td

td' :: El'
td' = td []

textarea :: El
textarea = elProps' D.textarea

textarea' :: El'
textarea' = textarea []

tfoot :: El
tfoot = elProps' D.tfoot

tfoot' :: El'
tfoot' = tfoot []

th :: El
th = elProps' D.th

th' :: El'
th' = th []

thead :: El
thead = elProps' D.thead

thead' :: El'
thead' = thead []

time :: El
time = elProps' D.time

time' :: El'
time' = time []

title :: El
title = elProps' D.title

title' :: El'
title' = title []

tr :: El
tr = elProps' D.tr

tr' :: El'
tr' = tr []

track :: El
track = elProps' D.track

track' :: El'
track' = track []

u :: El
u = elProps' D.u

u' :: El'
u' = u []

ul :: El
ul = elProps' D.ul

ul' :: El'
ul' = ul []

var :: El
var = elProps' D.var

var' :: El'
var' = var []

video :: El
video = elProps' D.video

video' :: El'
video' = video []

wbr :: El
wbr = elProps' D.body

wbr' :: El'
wbr' = wbr []
