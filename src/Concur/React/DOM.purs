module Concur.React.DOM where

import Prelude (pure, (<<<))
import Concur.React (el', display, Widget, HTML)
import React.DOM as D
import React.DOM.Props as P

-- Wrappers for all DOM elements from purescript-react
-- TODO: Generate these mechanically somehow

text :: forall a. String -> Widget HTML a
text = display <<< pure <<< D.text

type El = forall a. Array P.Props -> Array (Widget HTML a) -> Widget HTML a
type El' = forall a. Array (Widget HTML a) -> Widget HTML a

a :: El
a = el' <<< D.a

a' :: El'
a' = a []

abbr :: El
abbr = el' <<< D.abbr

abbr' :: El'
abbr' = abbr []

address :: El
address = el' <<< D.address

address' :: El'
address' = address []

area :: El
area = el' <<< D.area

area' :: El'
area' = area []

article :: El
article = el' <<< D.article

article' :: El'
article' = article []

aside :: El
aside = el' <<< D.aside

aside' :: El'
aside' = aside []

audio :: El
audio = el' <<< D.audio

audio' :: El'
audio' = audio []

b :: El
b = el' <<< D.b

b' :: El'
b' = b []

base :: El
base = el' <<< D.base

base' :: El'
base' = base []

bdi :: El
bdi = el' <<< D.bdi

bdi' :: El'
bdi' = bdi []

bdo :: El
bdo = el' <<< D.bdo

bdo' :: El'
bdo' = bdo []

big :: El
big = el' <<< D.big

big' :: El'
big' = big []

blockquote :: El
blockquote = el' <<< D.blockquote

blockquote' :: El'
blockquote' = blockquote []

body :: El
body = el' <<< D.body

body' :: El'
body' = body []

br :: El
br = el' <<< D.br

br' :: El'
br' = br []

button :: El
button = el' <<< D.button

button' :: El'
button' = button []

canvas :: El
canvas = el' <<< D.canvas

canvas' :: El'
canvas' = canvas []

caption :: El
caption = el' <<< D.caption

caption' :: El'
caption' = caption []

cite :: El
cite = el' <<< D.cite

cite' :: El'
cite' = cite []

code :: El
code = el' <<< D.code

code' :: El'
code' = code []

col :: El
col = el' <<< D.col

col' :: El'
col' = col []

colgroup :: El
colgroup = el' <<< D.colgroup

colgroup' :: El'
colgroup' = colgroup []

_data :: El
_data = el' <<< D._data

_data' :: El'
_data' = _data []

datalist :: El
datalist = el' <<< D.datalist

datalist' :: El'
datalist' = datalist []

dd :: El
dd = el' <<< D.dd

dd' :: El'
dd' = dd []

del :: El
del = el' <<< D.del

del' :: El'
del' = del []

details :: El
details = el' <<< D.details

details' :: El'
details' = details []

dfn :: El
dfn = el' <<< D.dfn

dfn' :: El'
dfn' = dfn []

dialog :: El
dialog = el' <<< D.dialog

dialog' :: El'
dialog' = dialog []

div :: El
div = el' <<< D.div

div' :: El'
div' = div []

dl :: El
dl = el' <<< D.dl

dl' :: El'
dl' = dl []

dt :: El
dt = el' <<< D.dt

dt' :: El'
dt' = dt []

em :: El
em = el' <<< D.em

em' :: El'
em' = em []

embed :: El
embed = el' <<< D.embed

embed' :: El'
embed' = embed []

fieldset :: El
fieldset = el' <<< D.fieldset

fieldset' :: El'
fieldset' = fieldset []

figcaption :: El
figcaption = el' <<< D.figcaption

figcaption' :: El'
figcaption' = figcaption []

figure :: El
figure = el' <<< D.figure

figure' :: El'
figure' = figure []

footer :: El
footer = el' <<< D.footer

footer' :: El'
footer' = footer []

form :: El
form = el' <<< D.form

form' :: El'
form' = form []

h1 :: El
h1 = el' <<< D.h1

h1' :: El'
h1' = h1 []

h2 :: El
h2 = el' <<< D.h2

h2' :: El'
h2' = h2 []

h3 :: El
h3 = el' <<< D.h3

h3' :: El'
h3' = h3 []

h4 :: El
h4 = el' <<< D.h4

h4' :: El'
h4' = h4 []

h5 :: El
h5 = el' <<< D.h5

h5' :: El'
h5' = h5 []

h6 :: El
h6 = el' <<< D.h6

h6' :: El'
h6' = h6 []

head :: El
head = el' <<< D.head

head' :: El'
head' = head []

header :: El
header = el' <<< D.header

header' :: El'
header' = header []

hr :: El
hr = el' <<< D.hr

hr' :: El'
hr' = hr []

html :: El
html = el' <<< D.html

html' :: El'
html' = html []

i :: El
i = el' <<< D.i

i' :: El'
i' = i []

iframe :: El
iframe = el' <<< D.iframe

iframe' :: El'
iframe' = iframe []

img :: El
img = el' <<< D.img

img' :: El'
img' = img []

input :: El
input = el' <<< D.input

input' :: El'
input' = input []

ins :: El
ins = el' <<< D.ins

ins' :: El'
ins' = ins []

kbd :: El
kbd = el' <<< D.kbd

kbd' :: El'
kbd' = kbd []

keygen :: El
keygen = el' <<< D.keygen

keygen' :: El'
keygen' = keygen []

label :: El
label = el' <<< D.label

label' :: El'
label' = label []

legend :: El
legend = el' <<< D.legend

legend' :: El'
legend' = legend []

li :: El
li = el' <<< D.li

li' :: El'
li' = li []

link :: El
link = el' <<< D.link

link' :: El'
link' = body []

main :: El
main = el' <<< D.main

main' :: El'
main' = main []

map :: El
map = el' <<< D.map

map' :: El'
map' = map []

mark :: El
mark = el' <<< D.mark

mark' :: El'
mark' = mark []

menu :: El
menu = el' <<< D.menu

menu' :: El'
menu' = menu []

menuitem :: El
menuitem = el' <<< D.menuitem

menuitem' :: El'
menuitem' = menuitem []

meta :: El
meta = el' <<< D.meta

meta' :: El'
meta' = meta []

meter :: El
meter = el' <<< D.meter

meter' :: El'
meter' = meter []

nav :: El
nav = el' <<< D.nav

nav' :: El'
nav' = nav []

noscript :: El
noscript = el' <<< D.noscript

noscript' :: El'
noscript' = noscript []

object :: El
object = el' <<< D.object

object' :: El'
object' = object []

ol :: El
ol = el' <<< D.ol

ol' :: El'
ol' = ol []

optgroup :: El
optgroup = el' <<< D.optgroup

optgroup' :: El'
optgroup' = optgroup []

option :: El
option = el' <<< D.option

option' :: El'
option' = option []

output :: El
output = el' <<< D.output

output' :: El'
output' = output []

p :: El
p = el' <<< D.p

p' :: El'
p' = p []

param :: El
param = el' <<< D.param

param' :: El'
param' = param []

picture :: El
picture = el' <<< D.picture

picture' :: El'
picture' = picture []

pre :: El
pre = el' <<< D.pre

pre' :: El'
pre' = pre []

progress :: El
progress = el' <<< D.progress

progress' :: El'
progress' = progress []

q :: El
q = el' <<< D.q

q' :: El'
q' = q []

rp :: El
rp = el' <<< D.rp

rp' :: El'
rp' = rp []

rt :: El
rt = el' <<< D.rt

rt' :: El'
rt' = rt []

ruby :: El
ruby = el' <<< D.ruby

ruby' :: El'
ruby' = ruby []

s :: El
s = el' <<< D.s

s' :: El'
s' = s []

samp :: El
samp = el' <<< D.samp

samp' :: El'
samp' = samp []

script :: El
script = el' <<< D.script

script' :: El'
script' = script []

section :: El
section = el' <<< D.section

section' :: El'
section' = section []

select :: El
select = el' <<< D.select

select' :: El'
select' = select []

small :: El
small = el' <<< D.small

small' :: El'
small' = small []

source :: El
source = el' <<< D.source

source' :: El'
source' = source []

span :: El
span = el' <<< D.span

span' :: El'
span' = span []

strong :: El
strong = el' <<< D.strong

strong' :: El'
strong' = strong []

style :: El
style = el' <<< D.style

style' :: El'
style' = style []

sub :: El
sub = el' <<< D.sub

sub' :: El'
sub' = sub []

summary :: El
summary = el' <<< D.summary

summary' :: El'
summary' = summary []

sup :: El
sup = el' <<< D.sup

sup' :: El'
sup' = sup []

table :: El
table = el' <<< D.table

table' :: El'
table' = table []

tbody :: El
tbody = el' <<< D.tbody

tbody' :: El'
tbody' = tbody []

td :: El
td = el' <<< D.td

td' :: El'
td' = td []

textarea :: El
textarea = el' <<< D.textarea

textarea' :: El'
textarea' = textarea []

tfoot :: El
tfoot = el' <<< D.tfoot

tfoot' :: El'
tfoot' = tfoot []

th :: El
th = el' <<< D.th

th' :: El'
th' = th []

thead :: El
thead = el' <<< D.thead

thead' :: El'
thead' = thead []

time :: El
time = el' <<< D.time

time' :: El'
time' = time []

title :: El
title = el' <<< D.title

title' :: El'
title' = title []

tr :: El
tr = el' <<< D.tr

tr' :: El'
tr' = tr []

track :: El
track = el' <<< D.track

track' :: El'
track' = track []

u :: El
u = el' <<< D.u

u' :: El'
u' = u []

ul :: El
ul = el' <<< D.ul

ul' :: El'
ul' = ul []

var :: El
var = el' <<< D.var

var' :: El'
var' = var []

video :: El
video = el' <<< D.video

video' :: El'
video' = video []

wbr :: El
wbr = el' <<< D.body

wbr' :: El'
wbr' = wbr []
