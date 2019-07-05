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
type El1
  = forall m a. ShiftMap (Widget HTML) m => Array (ReactProps a) -> m a -> m a

type El
  = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (ReactProps a) -> Array (m a) -> m a

type El'
  = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (m a) -> m a

type ElLeaf
  = forall m a. LiftWidget HTML m => Array (ReactProps a) -> m a

type ElLeaf'
  = forall m a. LiftWidget HTML m => m a

type ElLeafFunc' x
  = forall m a. LiftWidget HTML m => x -> m a

-------------------------------------------------------------------------------------------------------------------
text :: ElLeafFunc' String
text str = liftWidget $ display [D.text str]

int :: ElLeafFunc' Int
int x = liftWidget $ display [D.int x]

number :: ElLeafFunc' Number
number x = liftWidget $ display [D.number x]

a_ :: El1
a_ = el D.a

a :: El
a = el' D.a

a' :: El'
a' = a []

abbr_ :: El1
abbr_ = el D.abbr

abbr :: El
abbr = el' D.abbr

abbr' :: El'
abbr' = abbr []

address_ :: El1
address_ = el D.address

address :: El
address = el' D.address

address' :: El'
address' = address []

area :: ElLeaf
area = elLeaf D.area

area' :: ElLeaf'
area' = area []

article_ :: El1
article_ = el D.article

article :: El
article = el' D.article

article' :: El'
article' = article []

aside_ :: El1
aside_ = el D.aside

aside :: El
aside = el' D.aside

aside' :: El'
aside' = aside []

audio_ :: El1
audio_ = el D.audio

audio :: El
audio = el' D.audio

audio' :: El'
audio' = audio []

b_ :: El1
b_ = el D.b

b :: El
b = el' D.b

b' :: El'
b' = b []

base :: ElLeaf
base = elLeaf D.base

base' :: ElLeaf'
base' = base []

bdi_ :: El1
bdi_ = el D.bdi

bdi :: El
bdi = el' D.bdi

bdi' :: El'
bdi' = bdi []

bdo_ :: El1
bdo_ = el D.bdo

bdo :: El
bdo = el' D.bdo

bdo' :: El'
bdo' = bdo []

big_ :: El1
big_ = el D.big

big :: El
big = el' D.big

big' :: El'
big' = big []

blockquote_ :: El1
blockquote_ = el D.blockquote

blockquote :: El
blockquote = el' D.blockquote

blockquote' :: El'
blockquote' = blockquote []

body_ :: El1
body_ = el D.body

body :: El
body = el' D.body

body' :: El'
body' = body []

br :: ElLeaf
br = elLeaf D.br

br' :: ElLeaf'
br' = br []

button_ :: El1
button_ = el D.button

button :: El
button = el' D.button

button' :: El'
button' = button []

canvas_ :: El1
canvas_ = el D.canvas

canvas :: El
canvas = el' D.canvas

canvas' :: El'
canvas' = canvas []

caption_ :: El1
caption_ = el D.caption

caption :: El
caption = el' D.caption

caption' :: El'
caption' = caption []

cite_ :: El1
cite_ = el D.cite

cite :: El
cite = el' D.cite

cite' :: El'
cite' = cite []

code_ :: El1
code_ = el D.code

code :: El
code = el' D.code

code' :: El'
code' = code []

col :: ElLeaf
col = elLeaf D.col

col' :: ElLeaf'
col' = col []

colgroup_ :: El1
colgroup_ = el D.colgroup

colgroup :: El
colgroup = el' D.colgroup

colgroup' :: El'
colgroup' = colgroup []

_data_ :: El1
_data_ = el D._data

_data :: El
_data = el' D._data

_data' :: El'
_data' = _data []

datalist_ :: El1
datalist_ = el D.datalist

datalist :: El
datalist = el' D.datalist

datalist' :: El'
datalist' = datalist []

dd_ :: El1
dd_ = el D.dd

dd :: El
dd = el' D.dd

dd' :: El'
dd' = dd []

del_ :: El1
del_ = el D.del

del :: El
del = el' D.del

del' :: El'
del' = del []

details_ :: El1
details_ = el D.details

details :: El
details = el' D.details

details' :: El'
details' = details []

dfn_ :: El1
dfn_ = el D.dfn

dfn :: El
dfn = el' D.dfn

dfn' :: El'
dfn' = dfn []

dialog_ :: El1
dialog_ = el D.dialog

dialog :: El
dialog = el' D.dialog

dialog' :: El'
dialog' = dialog []

div_ :: El1
div_ = el D.div

div :: El
div = el' D.div

div' :: El'
div' = div []

dl_ :: El1
dl_ = el D.dl

dl :: El
dl = el' D.dl

dl' :: El'
dl' = dl []

dt_ :: El1
dt_ = el D.dt

dt :: El
dt = el' D.dt

dt' :: El'
dt' = dt []

em_ :: El1
em_ = el D.em

em :: El
em = el' D.em

em' :: El'
em' = em []

embed :: ElLeaf
embed = elLeaf D.embed

embed' :: ElLeaf'
embed' = embed []

fieldset_ :: El1
fieldset_ = el D.fieldset

fieldset :: El
fieldset = el' D.fieldset

fieldset' :: El'
fieldset' = fieldset []

figcaption_ :: El1
figcaption_ = el D.figcaption

figcaption :: El
figcaption = el' D.figcaption

figcaption' :: El'
figcaption' = figcaption []

figure_ :: El1
figure_ = el D.figure

figure :: El
figure = el' D.figure

figure' :: El'
figure' = figure []

footer_ :: El1
footer_ = el D.footer

footer :: El
footer = el' D.footer

footer' :: El'
footer' = footer []

form_ :: El1
form_ = el D.form

form :: El
form = el' D.form

form' :: El'
form' = form []

h1_ :: El1
h1_ = el D.h1

h1 :: El
h1 = el' D.h1

h1' :: El'
h1' = h1 []

h2_ :: El1
h2_ = el D.h2

h2 :: El
h2 = el' D.h2

h2' :: El'
h2' = h2 []

h3_ :: El1
h3_ = el D.h3

h3 :: El
h3 = el' D.h3

h3' :: El'
h3' = h3 []

h4_ :: El1
h4_ = el D.h4

h4 :: El
h4 = el' D.h4

h4' :: El'
h4' = h4 []

h5_ :: El1
h5_ = el D.h5

h5 :: El
h5 = el' D.h5

h5' :: El'
h5' = h5 []

h6_ :: El1
h6_ = el D.h6

h6 :: El
h6 = el' D.h6

h6' :: El'
h6' = h6 []

head_ :: El1
head_ = el D.head

head :: El
head = el' D.head

head' :: El'
head' = head []

header_ :: El1
header_ = el D.header

header :: El
header = el' D.header

header' :: El'
header' = header []

hr :: ElLeaf
hr = elLeaf D.hr

hr' :: ElLeaf'
hr' = hr []

html_ :: El1
html_ = el D.html

html :: El
html = el' D.html

html' :: El'
html' = html []

i_ :: El1
i_ = el D.i

i :: El
i = el' D.i

i' :: El'
i' = i []

iframe_ :: El1
iframe_ = el D.iframe

iframe :: El
iframe = el' D.iframe

iframe' :: El'
iframe' = iframe []

img :: ElLeaf
img = elLeaf D.img

img' :: ElLeaf'
img' = img []

input :: ElLeaf
input = elLeaf D.input

input' :: ElLeaf'
input' = input []

ins_ :: El1
ins_ = el D.ins

ins :: El
ins = el' D.ins

ins' :: El'
ins' = ins []

kbd_ :: El1
kbd_ = el D.kbd

kbd :: El
kbd = el' D.kbd

kbd' :: El'
kbd' = kbd []

keygen :: ElLeaf
keygen = elLeaf D.keygen

keygen' :: ElLeaf'
keygen' = keygen []

label_ :: El1
label_ = el D.label

label :: El
label = el' D.label

label' :: El'
label' = label []

legend_ :: El1
legend_ = el D.legend

legend :: El
legend = el' D.legend

legend' :: El'
legend' = legend []

li_ :: El1
li_ = el D.li

li :: El
li = el' D.li

li' :: El'
li' = li []

link :: ElLeaf
link = elLeaf D.link

link' :: ElLeaf'
link' = link []

main_ :: El1
main_ = el D.main

main :: El
main = el' D.main

main' :: El'
main' = main []

_map_ :: El1
_map_ = el D.map

_map :: El
_map = el' D.map

_map' :: El'
_map' = _map []

mark_ :: El1
mark_ = el D.mark

mark :: El
mark = el' D.mark

mark' :: El'
mark' = mark []

menu_ :: El1
menu_ = el D.menu

menu :: El
menu = el' D.menu

menu' :: El'
menu' = menu []

menuitem :: ElLeaf
menuitem = elLeaf D.menuitem

menuitem' :: ElLeaf'
menuitem' = menuitem []

meta :: ElLeaf
meta = elLeaf D.meta

meta' :: ElLeaf'
meta' = meta []

meter_ :: El1
meter_ = el D.meter

meter :: El
meter = el' D.meter

meter' :: El'
meter' = meter []

nav_ :: El1
nav_ = el D.nav

nav :: El
nav = el' D.nav

nav' :: El'
nav' = nav []

noscript_ :: El1
noscript_ = el D.noscript

noscript :: El
noscript = el' D.noscript

noscript' :: El'
noscript' = noscript []

object_ :: El1
object_ = el D.object

object :: El
object = el' D.object

object' :: El'
object' = object []

ol_ :: El1
ol_ = el D.ol

ol :: El
ol = el' D.ol

ol' :: El'
ol' = ol []

optgroup_ :: El1
optgroup_ = el D.optgroup

optgroup :: El
optgroup = el' D.optgroup

optgroup' :: El'
optgroup' = optgroup []

option_ :: El1
option_ = el D.option

option :: El
option = el' D.option

option' :: El'
option' = option []

output_ :: El1
output_ = el D.output

output :: El
output = el' D.output

output' :: El'
output' = output []

p_ :: El1
p_ = el D.p

p :: El
p = el' D.p

p' :: El'
p' = p []

param :: ElLeaf
param = elLeaf D.param

param' :: ElLeaf'
param' = param []

picture_ :: El1
picture_ = el D.picture

picture :: El
picture = el' D.picture

picture' :: El'
picture' = picture []

pre_ :: El1
pre_ = el D.pre

pre :: El
pre = el' D.pre

pre' :: El'
pre' = pre []

progress_ :: El1
progress_ = el D.progress

progress :: El
progress = el' D.progress

progress' :: El'
progress' = progress []

q_ :: El1
q_ = el D.q

q :: El
q = el' D.q

q' :: El'
q' = q []

rp_ :: El1
rp_ = el D.rp

rp :: El
rp = el' D.rp

rp' :: El'
rp' = rp []

rt_ :: El1
rt_ = el D.rt

rt :: El
rt = el' D.rt

rt' :: El'
rt' = rt []

ruby_ :: El1
ruby_ = el D.ruby

ruby :: El
ruby = el' D.ruby

ruby' :: El'
ruby' = ruby []

s_ :: El1
s_ = el D.s

s :: El
s = el' D.s

s' :: El'
s' = s []

samp_ :: El1
samp_ = el D.samp

samp :: El
samp = el' D.samp

samp' :: El'
samp' = samp []

script_ :: El1
script_ = el D.script

script :: El
script = el' D.script

script' :: El'
script' = script []

section_ :: El1
section_ = el D.section

section :: El
section = el' D.section

section' :: El'
section' = section []

select_ :: El1
select_ = el D.select

select :: El
select = el' D.select

select' :: El'
select' = select []

small_ :: El1
small_ = el D.small

small :: El
small = el' D.small

small' :: El'
small' = small []

source :: ElLeaf
source = elLeaf D.source

source' :: ElLeaf'
source' = source []

span_ :: El1
span_ = el D.span

span :: El
span = el' D.span

span' :: El'
span' = span []

strong_ :: El1
strong_ = el D.strong

strong :: El
strong = el' D.strong

strong' :: El'
strong' = strong []

style_ :: El1
style_ = el D.style

style :: El
style = el' D.style

style' :: El'
style' = style []

sub_ :: El1
sub_ = el D.sub

sub :: El
sub = el' D.sub

sub' :: El'
sub' = sub []

summary_ :: El1
summary_ = el D.summary

summary :: El
summary = el' D.summary

summary' :: El'
summary' = summary []

sup_ :: El1
sup_ = el D.sup

sup :: El
sup = el' D.sup

sup' :: El'
sup' = sup []

table_ :: El1
table_ = el D.table

table :: El
table = el' D.table

table' :: El'
table' = table []

tbody_ :: El1
tbody_ = el D.tbody

tbody :: El
tbody = el' D.tbody

tbody' :: El'
tbody' = tbody []

td_ :: El1
td_ = el D.td

td :: El
td = el' D.td

td' :: El'
td' = td []

textarea_ :: El1
textarea_ = el D.textarea

textarea :: El
textarea = el' D.textarea

textarea' :: El'
textarea' = textarea []

tfoot_ :: El1
tfoot_ = el D.tfoot

tfoot :: El
tfoot = el' D.tfoot

tfoot' :: El'
tfoot' = tfoot []

th_ :: El1
th_ = el D.th

th :: El
th = el' D.th

th' :: El'
th' = th []

thead_ :: El1
thead_ = el D.thead

thead :: El
thead = el' D.thead

thead' :: El'
thead' = thead []

time_ :: El1
time_ = el D.time

time :: El
time = el' D.time

time' :: El'
time' = time []

title_ :: El1
title_ = el D.title

title :: El
title = el' D.title

title' :: El'
title' = title []

tr_ :: El1
tr_ = el D.tr

tr :: El
tr = el' D.tr

tr' :: El'
tr' = tr []

track :: ElLeaf
track = elLeaf D.track

track' :: ElLeaf'
track' = track []

u_ :: El1
u_ = el D.u

u :: El
u = el' D.u

u' :: El'
u' = u []

ul_ :: El1
ul_ = el D.ul

ul :: El
ul = el' D.ul

ul' :: El'
ul' = ul []

var_ :: El1
var_ = el D.var

var :: El
var = el' D.var

var' :: El'
var' = var []

video_ :: El1
video_ = el D.video

video :: El
video = el' D.video

video' :: El'
video' = video []

wbr :: ElLeaf
wbr = elLeaf D.wbr

wbr' :: ElLeaf'
wbr' = wbr []
