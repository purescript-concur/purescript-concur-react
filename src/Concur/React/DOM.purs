module Concur.React.DOM where

import Prelude hiding (div,map,sub)

import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Props (Props, mkProp)
import Concur.Core.Types (Widget, display, mkLeafWidget, mkNodeWidget)
import Concur.React.Props (ReactProps)
import Concur.React.Types (HTML)
import Control.MultiAlternative (class MultiAlternative, orr)
import Control.ShiftMap (class ShiftMap, shiftMap)
import Data.Functor (map)
import React.DOM as D

-- | The React backend uses Array to make view monoidal
-- | We use this view adapter to derive our specialised `el` functions
viewAdapter
  :: forall ps vs res
  .  (ps -> vs -> res)
  -> (ps -> vs -> Array res)
viewAdapter f = \ps vs -> [f ps vs]

leafViewAdapter
  :: forall ps res
  .  (ps -> res)
  -> (ps -> Array res)
leafViewAdapter f = \ps -> [f ps]

el
  :: forall m a p v
  .  ShiftMap (Widget v) m
  => (Array p -> v -> v)
  -> Array (Props p a)
  -> m a
  -> m a
el e props = shiftMap \f ->
  mkNodeWidget \h v ->
    e (map (mkProp h <<< map (pure <<< f)) props) v

el'
  :: forall m a p v
  .  ShiftMap (Widget v) m
  => MultiAlternative m
  => (Array p -> v -> v)
  -> Array (Props p a)
  -> Array (m a)
  -> m a
el' f ps ms = el f ps (orr ms)

elLeaf
  :: forall p v m a
  .  LiftWidget v m
  => (Array p -> v)
  -> Array (Props p a)
  -> m a
elLeaf e props = liftWidget $ mkLeafWidget \h ->
  e (map (mkProp h <<< map pure) props)

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
a_ = el $ viewAdapter D.a

a :: El
a = el' $ viewAdapter D.a

a' :: El'
a' = a []

abbr_ :: El1
abbr_ = el $ viewAdapter D.abbr

abbr :: El
abbr = el' $ viewAdapter D.abbr

abbr' :: El'
abbr' = abbr []

address_ :: El1
address_ = el $ viewAdapter D.address

address :: El
address = el' $ viewAdapter D.address

address' :: El'
address' = address []

area :: ElLeaf
area = elLeaf $ leafViewAdapter D.area

area' :: ElLeaf'
area' = area []

article_ :: El1
article_ = el $ viewAdapter D.article

article :: El
article = el' $ viewAdapter D.article

article' :: El'
article' = article []

aside_ :: El1
aside_ = el $ viewAdapter D.aside

aside :: El
aside = el' $ viewAdapter D.aside

aside' :: El'
aside' = aside []

audio_ :: El1
audio_ = el $ viewAdapter D.audio

audio :: El
audio = el' $ viewAdapter D.audio

audio' :: El'
audio' = audio []

b_ :: El1
b_ = el $ viewAdapter D.b

b :: El
b = el' $ viewAdapter D.b

b' :: El'
b' = b []

base :: ElLeaf
base = elLeaf $ leafViewAdapter D.base

base' :: ElLeaf'
base' = base []

bdi_ :: El1
bdi_ = el $ viewAdapter D.bdi

bdi :: El
bdi = el' $ viewAdapter D.bdi

bdi' :: El'
bdi' = bdi []

bdo_ :: El1
bdo_ = el $ viewAdapter D.bdo

bdo :: El
bdo = el' $ viewAdapter D.bdo

bdo' :: El'
bdo' = bdo []

big_ :: El1
big_ = el $ viewAdapter D.big

big :: El
big = el' $ viewAdapter D.big

big' :: El'
big' = big []

blockquote_ :: El1
blockquote_ = el $ viewAdapter D.blockquote

blockquote :: El
blockquote = el' $ viewAdapter D.blockquote

blockquote' :: El'
blockquote' = blockquote []

body_ :: El1
body_ = el $ viewAdapter D.body

body :: El
body = el' $ viewAdapter D.body

body' :: El'
body' = body []

br :: ElLeaf
br = elLeaf $ leafViewAdapter D.br

br' :: ElLeaf'
br' = br []

button_ :: El1
button_ = el $ viewAdapter D.button

button :: El
button = el' $ viewAdapter D.button

button' :: El'
button' = button []

canvas_ :: El1
canvas_ = el $ viewAdapter D.canvas

canvas :: El
canvas = el' $ viewAdapter D.canvas

canvas' :: El'
canvas' = canvas []

caption_ :: El1
caption_ = el $ viewAdapter D.caption

caption :: El
caption = el' $ viewAdapter D.caption

caption' :: El'
caption' = caption []

cite_ :: El1
cite_ = el $ viewAdapter D.cite

cite :: El
cite = el' $ viewAdapter D.cite

cite' :: El'
cite' = cite []

code_ :: El1
code_ = el $ viewAdapter D.code

code :: El
code = el' $ viewAdapter D.code

code' :: El'
code' = code []

col :: ElLeaf
col = elLeaf $ leafViewAdapter D.col

col' :: ElLeaf'
col' = col []

colgroup_ :: El1
colgroup_ = el $ viewAdapter D.colgroup

colgroup :: El
colgroup = el' $ viewAdapter D.colgroup

colgroup' :: El'
colgroup' = colgroup []

_data_ :: El1
_data_ = el $ viewAdapter D._data

_data :: El
_data = el' $ viewAdapter D._data

_data' :: El'
_data' = _data []

datalist_ :: El1
datalist_ = el $ viewAdapter D.datalist

datalist :: El
datalist = el' $ viewAdapter D.datalist

datalist' :: El'
datalist' = datalist []

dd_ :: El1
dd_ = el $ viewAdapter D.dd

dd :: El
dd = el' $ viewAdapter D.dd

dd' :: El'
dd' = dd []

del_ :: El1
del_ = el $ viewAdapter D.del

del :: El
del = el' $ viewAdapter D.del

del' :: El'
del' = del []

details_ :: El1
details_ = el $ viewAdapter D.details

details :: El
details = el' $ viewAdapter D.details

details' :: El'
details' = details []

dfn_ :: El1
dfn_ = el $ viewAdapter D.dfn

dfn :: El
dfn = el' $ viewAdapter D.dfn

dfn' :: El'
dfn' = dfn []

dialog_ :: El1
dialog_ = el $ viewAdapter D.dialog

dialog :: El
dialog = el' $ viewAdapter D.dialog

dialog' :: El'
dialog' = dialog []

div_ :: El1
div_ = el $ viewAdapter D.div

div :: El
div = el' $ viewAdapter D.div

div' :: El'
div' = div []

dl_ :: El1
dl_ = el $ viewAdapter D.dl

dl :: El
dl = el' $ viewAdapter D.dl

dl' :: El'
dl' = dl []

dt_ :: El1
dt_ = el $ viewAdapter D.dt

dt :: El
dt = el' $ viewAdapter D.dt

dt' :: El'
dt' = dt []

em_ :: El1
em_ = el $ viewAdapter D.em

em :: El
em = el' $ viewAdapter D.em

em' :: El'
em' = em []

embed :: ElLeaf
embed = elLeaf $ leafViewAdapter D.embed

embed' :: ElLeaf'
embed' = embed []

fieldset_ :: El1
fieldset_ = el $ viewAdapter D.fieldset

fieldset :: El
fieldset = el' $ viewAdapter D.fieldset

fieldset' :: El'
fieldset' = fieldset []

figcaption_ :: El1
figcaption_ = el $ viewAdapter D.figcaption

figcaption :: El
figcaption = el' $ viewAdapter D.figcaption

figcaption' :: El'
figcaption' = figcaption []

figure_ :: El1
figure_ = el $ viewAdapter D.figure

figure :: El
figure = el' $ viewAdapter D.figure

figure' :: El'
figure' = figure []

footer_ :: El1
footer_ = el $ viewAdapter D.footer

footer :: El
footer = el' $ viewAdapter D.footer

footer' :: El'
footer' = footer []

form_ :: El1
form_ = el $ viewAdapter D.form

form :: El
form = el' $ viewAdapter D.form

form' :: El'
form' = form []

h1_ :: El1
h1_ = el $ viewAdapter D.h1

h1 :: El
h1 = el' $ viewAdapter D.h1

h1' :: El'
h1' = h1 []

h2_ :: El1
h2_ = el $ viewAdapter D.h2

h2 :: El
h2 = el' $ viewAdapter D.h2

h2' :: El'
h2' = h2 []

h3_ :: El1
h3_ = el $ viewAdapter D.h3

h3 :: El
h3 = el' $ viewAdapter D.h3

h3' :: El'
h3' = h3 []

h4_ :: El1
h4_ = el $ viewAdapter D.h4

h4 :: El
h4 = el' $ viewAdapter D.h4

h4' :: El'
h4' = h4 []

h5_ :: El1
h5_ = el $ viewAdapter D.h5

h5 :: El
h5 = el' $ viewAdapter D.h5

h5' :: El'
h5' = h5 []

h6_ :: El1
h6_ = el $ viewAdapter D.h6

h6 :: El
h6 = el' $ viewAdapter D.h6

h6' :: El'
h6' = h6 []

head_ :: El1
head_ = el $ viewAdapter D.head

head :: El
head = el' $ viewAdapter D.head

head' :: El'
head' = head []

header_ :: El1
header_ = el $ viewAdapter D.header

header :: El
header = el' $ viewAdapter D.header

header' :: El'
header' = header []

hr :: ElLeaf
hr = elLeaf $ leafViewAdapter D.hr

hr' :: ElLeaf'
hr' = hr []

html_ :: El1
html_ = el $ viewAdapter D.html

html :: El
html = el' $ viewAdapter D.html

html' :: El'
html' = html []

i_ :: El1
i_ = el $ viewAdapter D.i

i :: El
i = el' $ viewAdapter D.i

i' :: El'
i' = i []

iframe_ :: El1
iframe_ = el $ viewAdapter D.iframe

iframe :: El
iframe = el' $ viewAdapter D.iframe

iframe' :: El'
iframe' = iframe []

img :: ElLeaf
img = elLeaf $ leafViewAdapter D.img

img' :: ElLeaf'
img' = img []

input :: ElLeaf
input = elLeaf $ leafViewAdapter D.input

input' :: ElLeaf'
input' = input []

ins_ :: El1
ins_ = el $ viewAdapter D.ins

ins :: El
ins = el' $ viewAdapter D.ins

ins' :: El'
ins' = ins []

kbd_ :: El1
kbd_ = el $ viewAdapter D.kbd

kbd :: El
kbd = el' $ viewAdapter D.kbd

kbd' :: El'
kbd' = kbd []

keygen :: ElLeaf
keygen = elLeaf $ leafViewAdapter D.keygen

keygen' :: ElLeaf'
keygen' = keygen []

label_ :: El1
label_ = el $ viewAdapter D.label

label :: El
label = el' $ viewAdapter D.label

label' :: El'
label' = label []

legend_ :: El1
legend_ = el $ viewAdapter D.legend

legend :: El
legend = el' $ viewAdapter D.legend

legend' :: El'
legend' = legend []

li_ :: El1
li_ = el $ viewAdapter D.li

li :: El
li = el' $ viewAdapter D.li

li' :: El'
li' = li []

link :: ElLeaf
link = elLeaf $ leafViewAdapter D.link

link' :: ElLeaf'
link' = link []

main_ :: El1
main_ = el $ viewAdapter D.main

main :: El
main = el' $ viewAdapter D.main

main' :: El'
main' = main []

_map_ :: El1
_map_ = el $ viewAdapter D.map

_map :: El
_map = el' $ viewAdapter D.map

_map' :: El'
_map' = _map []

mark_ :: El1
mark_ = el $ viewAdapter D.mark

mark :: El
mark = el' $ viewAdapter D.mark

mark' :: El'
mark' = mark []

menu_ :: El1
menu_ = el $ viewAdapter D.menu

menu :: El
menu = el' $ viewAdapter D.menu

menu' :: El'
menu' = menu []

menuitem :: ElLeaf
menuitem = elLeaf $ leafViewAdapter D.menuitem

menuitem' :: ElLeaf'
menuitem' = menuitem []

meta :: ElLeaf
meta = elLeaf $ leafViewAdapter D.meta

meta' :: ElLeaf'
meta' = meta []

meter_ :: El1
meter_ = el $ viewAdapter D.meter

meter :: El
meter = el' $ viewAdapter D.meter

meter' :: El'
meter' = meter []

nav_ :: El1
nav_ = el $ viewAdapter D.nav

nav :: El
nav = el' $ viewAdapter D.nav

nav' :: El'
nav' = nav []

noscript_ :: El1
noscript_ = el $ viewAdapter D.noscript

noscript :: El
noscript = el' $ viewAdapter D.noscript

noscript' :: El'
noscript' = noscript []

object_ :: El1
object_ = el $ viewAdapter D.object

object :: El
object = el' $ viewAdapter D.object

object' :: El'
object' = object []

ol_ :: El1
ol_ = el $ viewAdapter D.ol

ol :: El
ol = el' $ viewAdapter D.ol

ol' :: El'
ol' = ol []

optgroup_ :: El1
optgroup_ = el $ viewAdapter D.optgroup

optgroup :: El
optgroup = el' $ viewAdapter D.optgroup

optgroup' :: El'
optgroup' = optgroup []

option_ :: El1
option_ = el $ viewAdapter D.option

option :: El
option = el' $ viewAdapter D.option

option' :: El'
option' = option []

output_ :: El1
output_ = el $ viewAdapter D.output

output :: El
output = el' $ viewAdapter D.output

output' :: El'
output' = output []

p_ :: El1
p_ = el $ viewAdapter D.p

p :: El
p = el' $ viewAdapter D.p

p' :: El'
p' = p []

param :: ElLeaf
param = elLeaf $ leafViewAdapter D.param

param' :: ElLeaf'
param' = param []

picture_ :: El1
picture_ = el $ viewAdapter D.picture

picture :: El
picture = el' $ viewAdapter D.picture

picture' :: El'
picture' = picture []

pre_ :: El1
pre_ = el $ viewAdapter D.pre

pre :: El
pre = el' $ viewAdapter D.pre

pre' :: El'
pre' = pre []

progress_ :: El1
progress_ = el $ viewAdapter D.progress

progress :: El
progress = el' $ viewAdapter D.progress

progress' :: El'
progress' = progress []

q_ :: El1
q_ = el $ viewAdapter D.q

q :: El
q = el' $ viewAdapter D.q

q' :: El'
q' = q []

rp_ :: El1
rp_ = el $ viewAdapter D.rp

rp :: El
rp = el' $ viewAdapter D.rp

rp' :: El'
rp' = rp []

rt_ :: El1
rt_ = el $ viewAdapter D.rt

rt :: El
rt = el' $ viewAdapter D.rt

rt' :: El'
rt' = rt []

ruby_ :: El1
ruby_ = el $ viewAdapter D.ruby

ruby :: El
ruby = el' $ viewAdapter D.ruby

ruby' :: El'
ruby' = ruby []

s_ :: El1
s_ = el $ viewAdapter D.s

s :: El
s = el' $ viewAdapter D.s

s' :: El'
s' = s []

samp_ :: El1
samp_ = el $ viewAdapter D.samp

samp :: El
samp = el' $ viewAdapter D.samp

samp' :: El'
samp' = samp []

script_ :: El1
script_ = el $ viewAdapter D.script

script :: El
script = el' $ viewAdapter D.script

script' :: El'
script' = script []

section_ :: El1
section_ = el $ viewAdapter D.section

section :: El
section = el' $ viewAdapter D.section

section' :: El'
section' = section []

select_ :: El1
select_ = el $ viewAdapter D.select

select :: El
select = el' $ viewAdapter D.select

select' :: El'
select' = select []

small_ :: El1
small_ = el $ viewAdapter D.small

small :: El
small = el' $ viewAdapter D.small

small' :: El'
small' = small []

source :: ElLeaf
source = elLeaf $ leafViewAdapter D.source

source' :: ElLeaf'
source' = source []

span_ :: El1
span_ = el $ viewAdapter D.span

span :: El
span = el' $ viewAdapter D.span

span' :: El'
span' = span []

strong_ :: El1
strong_ = el $ viewAdapter D.strong

strong :: El
strong = el' $ viewAdapter D.strong

strong' :: El'
strong' = strong []

style_ :: El1
style_ = el $ viewAdapter D.style

style :: El
style = el' $ viewAdapter D.style

style' :: El'
style' = style []

sub_ :: El1
sub_ = el $ viewAdapter D.sub

sub :: El
sub = el' $ viewAdapter D.sub

sub' :: El'
sub' = sub []

summary_ :: El1
summary_ = el $ viewAdapter D.summary

summary :: El
summary = el' $ viewAdapter D.summary

summary' :: El'
summary' = summary []

sup_ :: El1
sup_ = el $ viewAdapter D.sup

sup :: El
sup = el' $ viewAdapter D.sup

sup' :: El'
sup' = sup []

table_ :: El1
table_ = el $ viewAdapter D.table

table :: El
table = el' $ viewAdapter D.table

table' :: El'
table' = table []

tbody_ :: El1
tbody_ = el $ viewAdapter D.tbody

tbody :: El
tbody = el' $ viewAdapter D.tbody

tbody' :: El'
tbody' = tbody []

td_ :: El1
td_ = el $ viewAdapter D.td

td :: El
td = el' $ viewAdapter D.td

td' :: El'
td' = td []

textarea_ :: El1
textarea_ = el $ viewAdapter D.textarea

textarea :: El
textarea = el' $ viewAdapter D.textarea

textarea' :: El'
textarea' = textarea []

tfoot_ :: El1
tfoot_ = el $ viewAdapter D.tfoot

tfoot :: El
tfoot = el' $ viewAdapter D.tfoot

tfoot' :: El'
tfoot' = tfoot []

th_ :: El1
th_ = el $ viewAdapter D.th

th :: El
th = el' $ viewAdapter D.th

th' :: El'
th' = th []

thead_ :: El1
thead_ = el $ viewAdapter D.thead

thead :: El
thead = el' $ viewAdapter D.thead

thead' :: El'
thead' = thead []

time_ :: El1
time_ = el $ viewAdapter D.time

time :: El
time = el' $ viewAdapter D.time

time' :: El'
time' = time []

title_ :: El1
title_ = el $ viewAdapter D.title

title :: El
title = el' $ viewAdapter D.title

title' :: El'
title' = title []

tr_ :: El1
tr_ = el $ viewAdapter D.tr

tr :: El
tr = el' $ viewAdapter D.tr

tr' :: El'
tr' = tr []

track :: ElLeaf
track = elLeaf $ leafViewAdapter D.track

track' :: ElLeaf'
track' = track []

u_ :: El1
u_ = el $ viewAdapter D.u

u :: El
u = el' $ viewAdapter D.u

u' :: El'
u' = u []

ul_ :: El1
ul_ = el $ viewAdapter D.ul

ul :: El
ul = el' $ viewAdapter D.ul

ul' :: El'
ul' = ul []

var_ :: El1
var_ = el $ viewAdapter D.var

var :: El
var = el' $ viewAdapter D.var

var' :: El'
var' = var []

video_ :: El1
video_ = el $ viewAdapter D.video

video :: El
video = el' $ viewAdapter D.video

video' :: El'
video' = video []

wbr :: ElLeaf
wbr = elLeaf $ leafViewAdapter D.wbr

wbr' :: ElLeaf'
wbr' = wbr []
