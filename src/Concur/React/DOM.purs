module Concur.React.DOM where

import Prelude hiding (div,map,sub)

import Concur.Core.DOM (el, el', elLeaf) as CD
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Props (Props)
import Concur.Core.Types (Widget, display)
import Concur.React (HTML)
import Concur.React.Props (ReactProps)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import React.DOM as D

-- | The React backend uses Array to make view monoidal
-- | We use this view adapter to derive our specialised `el` functions
viewAdapter
  :: forall ps vs res
  .  (ps -> vs -> res)
  -> (ps -> vs -> Array res)
viewAdapter f = \ps vs -> [f ps vs]

el
  :: forall m a p v
  .  ShiftMap (Widget (Array v)) m
  => (Array p -> Array v -> v)
  -> Array (Props p a)
  -> m a
  -> m a
el f = CD.el (viewAdapter f)

el'
  :: forall m a p v
  .  ShiftMap (Widget (Array v)) m
  => MultiAlternative m
  => (Array p -> Array v -> v)
  -> Array (Props p a)
  -> Array (m a)
  -> m a
el' f = CD.el' (viewAdapter f)

elLeaf
  :: forall p v m a
  .  LiftWidget (Array v) m
  => (Array p -> v)
  -> Array (Props p a)
  -> m a
elLeaf f = CD.elLeaf (\ps -> [f ps])

-- Wrappers for all DOM elements from purescript-react
-- TODO: Generate these mechanically somehow
type El
  = forall m a. ShiftMap (Widget HTML) m => Array (ReactProps a) -> m a -> m a

type El_
  = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => m a -> m a

type ElLeaf
  = forall m a. LiftWidget HTML m => Array (ReactProps a) -> m a

type ElLeaf_
  = forall m a. LiftWidget HTML m => m a

type ElLeafFunc_ x
  = forall m a. LiftWidget HTML m => x -> m a

-------------------------------------------------------------------------------------------------------------------
text :: ElLeafFunc_ String
text str = liftWidget $ display [D.text str]

int :: ElLeafFunc_ Int
int x = liftWidget $ display [D.int x]

number :: ElLeafFunc_ Number
number x = liftWidget $ display [D.number x]

a :: El
a = el D.a

a_ :: El_
a_ = a []

abbr :: El
abbr = el D.abbr

abbr_ :: El_
abbr_ = abbr []

address :: El
address = el D.address

address_ :: El_
address_ = address []

area :: ElLeaf
area = elLeaf D.area

area_ :: ElLeaf_
area_ = area []

article :: El
article = el D.article

article_ :: El_
article_ = article []

aside :: El
aside = el D.aside

aside_ :: El_
aside_ = aside []

audio :: El
audio = el D.audio

audio_ :: El_
audio_ = audio []

b :: El
b = el D.b

b_ :: El_
b_ = b []

base :: ElLeaf
base = elLeaf D.base

base_ :: ElLeaf_
base_ = base []

bdi :: El
bdi = el D.bdi

bdi_ :: El_
bdi_ = bdi []

bdo :: El
bdo = el D.bdo

bdo_ :: El_
bdo_ = bdo []

big :: El
big = el D.big

big_ :: El_
big_ = big []

blockquote :: El
blockquote = el D.blockquote

blockquote_ :: El_
blockquote_ = blockquote []

body :: El
body = el D.body

body_ :: El_
body_ = body []

br :: ElLeaf
br = elLeaf D.br

br_ :: ElLeaf_
br_ = br []

button :: El
button = el D.button

button_ :: El_
button_ = button []

canvas :: El
canvas = el D.canvas

canvas_ :: El_
canvas_ = canvas []

caption :: El
caption = el D.caption

caption_ :: El_
caption_ = caption []

cite :: El
cite = el D.cite

cite_ :: El_
cite_ = cite []

code :: El
code = el D.code

code_ :: El_
code_ = code []

col :: ElLeaf
col = elLeaf D.col

col_ :: ElLeaf_
col_ = col []

colgroup :: El
colgroup = el D.colgroup

colgroup_ :: El_
colgroup_ = colgroup []

_data :: El
_data = el D._data

_data_ :: El_
_data_ = _data []

datalist :: El
datalist = el D.datalist

datalist_ :: El_
datalist_ = datalist []

dd :: El
dd = el D.dd

dd_ :: El_
dd_ = dd []

del :: El
del = el D.del

del_ :: El_
del_ = del []

details :: El
details = el D.details

details_ :: El_
details_ = details []

dfn :: El
dfn = el D.dfn

dfn_ :: El_
dfn_ = dfn []

dialog :: El
dialog = el D.dialog

dialog_ :: El_
dialog_ = dialog []

div :: El
div = el D.div

div_ :: El_
div_ = div []

dl :: El
dl = el D.dl

dl_ :: El_
dl_ = dl []

dt :: El
dt = el D.dt

dt_ :: El_
dt_ = dt []

em :: El
em = el D.em

em_ :: El_
em_ = em []

embed :: ElLeaf
embed = elLeaf D.embed

embed_ :: ElLeaf_
embed_ = embed []

fieldset :: El
fieldset = el D.fieldset

fieldset_ :: El_
fieldset_ = fieldset []

figcaption :: El
figcaption = el D.figcaption

figcaption_ :: El_
figcaption_ = figcaption []

figure :: El
figure = el D.figure

figure_ :: El_
figure_ = figure []

footer :: El
footer = el D.footer

footer_ :: El_
footer_ = footer []

form :: El
form = el D.form

form_ :: El_
form_ = form []

h1 :: El
h1 = el D.h1

h1_ :: El_
h1_ = h1 []

h2 :: El
h2 = el D.h2

h2_ :: El_
h2_ = h2 []

h3 :: El
h3 = el D.h3

h3_ :: El_
h3_ = h3 []

h4 :: El
h4 = el D.h4

h4_ :: El_
h4_ = h4 []

h5 :: El
h5 = el D.h5

h5_ :: El_
h5_ = h5 []

h6 :: El
h6 = el D.h6

h6_ :: El_
h6_ = h6 []

head :: El
head = el D.head

head_ :: El_
head_ = head []

header :: El
header = el D.header

header_ :: El_
header_ = header []

hr :: ElLeaf
hr = elLeaf D.hr

hr_ :: ElLeaf_
hr_ = hr []

html :: El
html = el D.html

html_ :: El_
html_ = html []

i :: El
i = el D.i

i_ :: El_
i_ = i []

iframe :: El
iframe = el D.iframe

iframe_ :: El_
iframe_ = iframe []

img :: ElLeaf
img = elLeaf D.img

img_ :: ElLeaf_
img_ = img []

input :: ElLeaf
input = elLeaf D.input

input_ :: ElLeaf_
input_ = input []

ins :: El
ins = el D.ins

ins_ :: El_
ins_ = ins []

kbd :: El
kbd = el D.kbd

kbd_ :: El_
kbd_ = kbd []

keygen :: ElLeaf
keygen = elLeaf D.keygen

keygen_ :: ElLeaf_
keygen_ = keygen []

label :: El
label = el D.label

label_ :: El_
label_ = label []

legend :: El
legend = el D.legend

legend_ :: El_
legend_ = legend []

li :: El
li = el D.li

li_ :: El_
li_ = li []

link :: ElLeaf
link = elLeaf D.link

link_ :: ElLeaf_
link_ = link []

main :: El
main = el D.main

main_ :: El_
main_ = main []

_map :: El
_map = el D.map

_map_ :: El_
_map_ = _map []

mark :: El
mark = el D.mark

mark_ :: El_
mark_ = mark []

menu :: El
menu = el D.menu

menu_ :: El_
menu_ = menu []

menuitem :: ElLeaf
menuitem = elLeaf D.menuitem

menuitem_ :: ElLeaf_
menuitem_ = menuitem []

meta :: ElLeaf
meta = elLeaf D.meta

meta_ :: ElLeaf_
meta_ = meta []

meter :: El
meter = el D.meter

meter_ :: El_
meter_ = meter []

nav :: El
nav = el D.nav

nav_ :: El_
nav_ = nav []

noscript :: El
noscript = el D.noscript

noscript_ :: El_
noscript_ = noscript []

object :: El
object = el D.object

object_ :: El_
object_ = object []

ol :: El
ol = el D.ol

ol_ :: El_
ol_ = ol []

optgroup :: El
optgroup = el D.optgroup

optgroup_ :: El_
optgroup_ = optgroup []

option :: El
option = el D.option

option_ :: El_
option_ = option []

output :: El
output = el D.output

output_ :: El_
output_ = output []

p :: El
p = el D.p

p_ :: El_
p_ = p []

param :: ElLeaf
param = elLeaf D.param

param_ :: ElLeaf_
param_ = param []

picture :: El
picture = el D.picture

picture_ :: El_
picture_ = picture []

pre :: El
pre = el D.pre

pre_ :: El_
pre_ = pre []

progress :: El
progress = el D.progress

progress_ :: El_
progress_ = progress []

q :: El
q = el D.q

q_ :: El_
q_ = q []

rp :: El
rp = el D.rp

rp_ :: El_
rp_ = rp []

rt :: El
rt = el D.rt

rt_ :: El_
rt_ = rt []

ruby :: El
ruby = el D.ruby

ruby_ :: El_
ruby_ = ruby []

s :: El
s = el D.s

s_ :: El_
s_ = s []

samp :: El
samp = el D.samp

samp_ :: El_
samp_ = samp []

script :: El
script = el D.script

script_ :: El_
script_ = script []

section :: El
section = el D.section

section_ :: El_
section_ = section []

select :: El
select = el D.select

select_ :: El_
select_ = select []

small :: El
small = el D.small

small_ :: El_
small_ = small []

source :: ElLeaf
source = elLeaf D.source

source_ :: ElLeaf_
source_ = source []

span :: El
span = el D.span

span_ :: El_
span_ = span []

strong :: El
strong = el D.strong

strong_ :: El_
strong_ = strong []

style :: El
style = el D.style

style_ :: El_
style_ = style []

sub :: El
sub = el D.sub

sub_ :: El_
sub_ = sub []

summary :: El
summary = el D.summary

summary_ :: El_
summary_ = summary []

sup :: El
sup = el D.sup

sup_ :: El_
sup_ = sup []

table :: El
table = el D.table

table_ :: El_
table_ = table []

tbody :: El
tbody = el D.tbody

tbody_ :: El_
tbody_ = tbody []

td :: El
td = el D.td

td_ :: El_
td_ = td []

textarea :: El
textarea = el D.textarea

textarea_ :: El_
textarea_ = textarea []

tfoot :: El
tfoot = el D.tfoot

tfoot_ :: El_
tfoot_ = tfoot []

th :: El
th = el D.th

th_ :: El_
th_ = th []

thead :: El
thead = el D.thead

thead_ :: El_
thead_ = thead []

time :: El
time = el D.time

time_ :: El_
time_ = time []

title :: El
title = el D.title

title_ :: El_
title_ = title []

tr :: El
tr = el D.tr

tr_ :: El_
tr_ = tr []

track :: ElLeaf
track = elLeaf D.track

track_ :: ElLeaf_
track_ = track []

u :: El
u = el D.u

u_ :: El_
u_ = u []

ul :: El
ul = el D.ul

ul_ :: El_
ul_ = ul []

var :: El
var = el D.var

var_ :: El_
var_ = var []

video :: El
video = el D.video

video_ :: El_
video_ = video []

wbr :: ElLeaf
wbr = elLeaf D.wbr

wbr_ :: ElLeaf_
wbr_ = wbr []
