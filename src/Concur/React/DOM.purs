module Concur.React.DOM where

import Concur.Core (display, Widget)
import Concur.React (HTML, el, el')
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Prelude (pure, (<<<))
import React.DOM as D
import React.DOM.Props as P

-- Wrappers for all DOM elements from purescript-react
-- TODO: Generate these mechanically somehow

text :: forall a. String -> Widget HTML a
text = display <<< pure <<< D.text

type El = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array P.Props -> Array (m a) -> m a
type El' = forall m a. MultiAlternative m => ShiftMap (Widget HTML) m => Array (m a) -> m a
type El_ = forall m a. ShiftMap (Widget HTML) m => Array P.Props -> m a -> m a
type El'_ = forall m a. ShiftMap (Widget HTML) m => m a -> m a

a :: El
a = el' <<< D.a

a' :: El'
a' = a []

a_ :: El_
a_ = el <<< D.a

a'_ :: El'_
a'_ = a_ []

abbr :: El
abbr = el' <<< D.abbr

abbr' :: El'
abbr' = abbr []

abbr_ :: El_
abbr_ = el <<< D.abbr

abbr'_ :: El'_
abbr'_ = abbr_ []

address :: El
address = el' <<< D.address

address' :: El'
address' = address []

address_ :: El_
address_ = el <<< D.address

address'_ :: El'_
address'_ = address_ []

area :: El
area = el' <<< D.area

area' :: El'
area' = area []

area_ :: El_
area_ = el <<< D.area

area'_ :: El'_
area'_ = area_ []

article :: El
article = el' <<< D.article

article' :: El'
article' = article []

article_ :: El_
article_ = el <<< D.article

article'_ :: El'_
article'_ = article_ []

aside :: El
aside = el' <<< D.aside

aside' :: El'
aside' = aside []

aside_ :: El_
aside_ = el <<< D.aside

aside'_ :: El'_
aside'_ = aside_ []

audio :: El
audio = el' <<< D.audio

audio' :: El'
audio' = audio []

audio_ :: El_
audio_ = el <<< D.audio

audio'_ :: El'_
audio'_ = audio_ []

b :: El
b = el' <<< D.b

b' :: El'
b' = b []

b_ :: El_
b_ = el <<< D.b

b'_ :: El'_
b'_ = b_ []

base :: El
base = el' <<< D.base

base' :: El'
base' = base []

base_ :: El_
base_ = el <<< D.base

base'_ :: El'_
base'_ = base_ []

bdi :: El
bdi = el' <<< D.bdi

bdi' :: El'
bdi' = bdi []

bdi_ :: El_
bdi_ = el <<< D.bdi

bdi'_ :: El'_
bdi'_ = bdi_ []

bdo :: El
bdo = el' <<< D.bdo

bdo' :: El'
bdo' = bdo []

bdo_ :: El_
bdo_ = el <<< D.bdo

bdo'_ :: El'_
bdo'_ = bdo_ []

big :: El
big = el' <<< D.big

big' :: El'
big' = big []

big_ :: El_
big_ = el <<< D.big

big'_ :: El'_
big'_ = big_ []

blockquote :: El
blockquote = el' <<< D.blockquote

blockquote' :: El'
blockquote' = blockquote []

blockquote_ :: El_
blockquote_ = el <<< D.blockquote

blockquote'_ :: El'_
blockquote'_ = blockquote_ []

body :: El
body = el' <<< D.body

body' :: El'
body' = body []

body_ :: El_
body_ = el <<< D.body

body'_ :: El'_
body'_ = body_ []

br :: El
br = el' <<< D.br

br' :: El'
br' = br []

br_ :: El_
br_ = el <<< D.br

br'_ :: El'_
br'_ = br_ []

button :: El
button = el' <<< D.button

button' :: El'
button' = button []

button_ :: El_
button_ = el <<< D.button

button'_ :: El'_
button'_ = button_ []

canvas :: El
canvas = el' <<< D.canvas

canvas' :: El'
canvas' = canvas []

canvas_ :: El_
canvas_ = el <<< D.canvas

canvas'_ :: El'_
canvas'_ = canvas_ []

caption :: El
caption = el' <<< D.caption

caption' :: El'
caption' = caption []

caption_ :: El_
caption_ = el <<< D.caption

caption'_ :: El'_
caption'_ = caption_ []

cite :: El
cite = el' <<< D.cite

cite' :: El'
cite' = cite []

cite_ :: El_
cite_ = el <<< D.cite

cite'_ :: El'_
cite'_ = cite_ []

code :: El
code = el' <<< D.code

code' :: El'
code' = code []

code_ :: El_
code_ = el <<< D.code

code'_ :: El'_
code'_ = code_ []

col :: El
col = el' <<< D.col

col' :: El'
col' = col []

col_ :: El_
col_ = el <<< D.col

col'_ :: El'_
col'_ = col_ []

colgroup :: El
colgroup = el' <<< D.colgroup

colgroup' :: El'
colgroup' = colgroup []

colgroup_ :: El_
colgroup_ = el <<< D.colgroup

colgroup'_ :: El'_
colgroup'_ = colgroup_ []

_data :: El
_data = el' <<< D._data

_data' :: El'
_data' = _data []

_data_ :: El_
_data_ = el <<< D._data

_data'_ :: El'_
_data'_ = _data_ []

datalist :: El
datalist = el' <<< D.datalist

datalist' :: El'
datalist' = datalist []

datalist_ :: El_
datalist_ = el <<< D.datalist

datalist'_ :: El'_
datalist'_ = datalist_ []

dd :: El
dd = el' <<< D.dd

dd' :: El'
dd' = dd []

dd_ :: El_
dd_ = el <<< D.dd

dd'_ :: El'_
dd'_ = dd_ []

del :: El
del = el' <<< D.del

del' :: El'
del' = del []

del_ :: El_
del_ = el <<< D.del

del'_ :: El'_
del'_ = del_ []

details :: El
details = el' <<< D.details

details' :: El'
details' = details []

details_ :: El_
details_ = el <<< D.details

details'_ :: El'_
details'_ = details_ []

dfn :: El
dfn = el' <<< D.dfn

dfn' :: El'
dfn' = dfn []

dfn_ :: El_
dfn_ = el <<< D.dfn

dfn'_ :: El'_
dfn'_ = dfn_ []

dialog :: El
dialog = el' <<< D.dialog

dialog' :: El'
dialog' = dialog []

dialog_ :: El_
dialog_ = el <<< D.dialog

dialog'_ :: El'_
dialog'_ = dialog_ []

div :: El
div = el' <<< D.div

div' :: El'
div' = div []

div_ :: El_
div_ = el <<< D.div

div'_ :: El'_
div'_ = div_ []

dl :: El
dl = el' <<< D.dl

dl' :: El'
dl' = dl []

dl_ :: El_
dl_ = el <<< D.dl

dl'_ :: El'_
dl'_ = dl_ []

dt :: El
dt = el' <<< D.dt

dt' :: El'
dt' = dt []

dt_ :: El_
dt_ = el <<< D.dt

dt'_ :: El'_
dt'_ = dt_ []

em :: El
em = el' <<< D.em

em' :: El'
em' = em []

em_ :: El_
em_ = el <<< D.em

em'_ :: El'_
em'_ = em_ []

embed :: El
embed = el' <<< D.embed

embed' :: El'
embed' = embed []

embed_ :: El_
embed_ = el <<< D.embed

embed'_ :: El'_
embed'_ = embed_ []

fieldset :: El
fieldset = el' <<< D.fieldset

fieldset' :: El'
fieldset' = fieldset []

fieldset_ :: El_
fieldset_ = el <<< D.fieldset

fieldset'_ :: El'_
fieldset'_ = fieldset_ []

figcaption :: El
figcaption = el' <<< D.figcaption

figcaption' :: El'
figcaption' = figcaption []

figcaption_ :: El_
figcaption_ = el <<< D.figcaption

figcaption'_ :: El'_
figcaption'_ = figcaption_ []

figure :: El
figure = el' <<< D.figure

figure' :: El'
figure' = figure []

figure_ :: El_
figure_ = el <<< D.figure

figure'_ :: El'_
figure'_ = figure_ []

footer :: El
footer = el' <<< D.footer

footer' :: El'
footer' = footer []

footer_ :: El_
footer_ = el <<< D.footer

footer'_ :: El'_
footer'_ = footer_ []

form :: El
form = el' <<< D.form

form' :: El'
form' = form []

form_ :: El_
form_ = el <<< D.form

form'_ :: El'_
form'_ = form_ []

h1 :: El
h1 = el' <<< D.h1

h1' :: El'
h1' = h1 []

h1_ :: El_
h1_ = el <<< D.h1

h1'_ :: El'_
h1'_ = h1_ []

h2 :: El
h2 = el' <<< D.h2

h2' :: El'
h2' = h2 []

h2_ :: El_
h2_ = el <<< D.h2

h2'_ :: El'_
h2'_ = h2_ []

h3 :: El
h3 = el' <<< D.h3

h3' :: El'
h3' = h3 []

h3_ :: El_
h3_ = el <<< D.h3

h3'_ :: El'_
h3'_ = h3_ []

h4 :: El
h4 = el' <<< D.h4

h4' :: El'
h4' = h4 []

h4_ :: El_
h4_ = el <<< D.h4

h4'_ :: El'_
h4'_ = h4_ []

h5 :: El
h5 = el' <<< D.h5

h5' :: El'
h5' = h5 []

h5_ :: El_
h5_ = el <<< D.h5

h5'_ :: El'_
h5'_ = h5_ []

h6 :: El
h6 = el' <<< D.h6

h6' :: El'
h6' = h6 []

h6_ :: El_
h6_ = el <<< D.h6

h6'_ :: El'_
h6'_ = h6_ []

head :: El
head = el' <<< D.head

head' :: El'
head' = head []

head_ :: El_
head_ = el <<< D.head

head'_ :: El'_
head'_ = head_ []

header :: El
header = el' <<< D.header

header' :: El'
header' = header []

header_ :: El_
header_ = el <<< D.header

header'_ :: El'_
header'_ = header_ []

hr :: El
hr = el' <<< D.hr

hr' :: El'
hr' = hr []

hr_ :: El_
hr_ = el <<< D.hr

hr'_ :: El'_
hr'_ = hr_ []

html :: El
html = el' <<< D.html

html' :: El'
html' = html []

html_ :: El_
html_ = el <<< D.html

html'_ :: El'_
html'_ = html_ []

i :: El
i = el' <<< D.i

i' :: El'
i' = i []

i_ :: El_
i_ = el <<< D.i

i'_ :: El'_
i'_ = i_ []

iframe :: El
iframe = el' <<< D.iframe

iframe' :: El'
iframe' = iframe []

iframe_ :: El_
iframe_ = el <<< D.iframe

iframe'_ :: El'_
iframe'_ = iframe_ []

img :: El
img = el' <<< D.img

img' :: El'
img' = img []

img_ :: El_
img_ = el <<< D.img

img'_ :: El'_
img'_ = img_ []

input :: El
input = el' <<< D.input

input' :: El'
input' = input []

input_ :: El_
input_ = el <<< D.input

input'_ :: El'_
input'_ = input_ []

ins :: El
ins = el' <<< D.ins

ins' :: El'
ins' = ins []

ins_ :: El_
ins_ = el <<< D.ins

ins'_ :: El'_
ins'_ = ins_ []

kbd :: El
kbd = el' <<< D.kbd

kbd' :: El'
kbd' = kbd []

kbd_ :: El_
kbd_ = el <<< D.kbd

kbd'_ :: El'_
kbd'_ = kbd_ []

keygen :: El
keygen = el' <<< D.keygen

keygen' :: El'
keygen' = keygen []

keygen_ :: El_
keygen_ = el <<< D.keygen

keygen'_ :: El'_
keygen'_ = keygen_ []

label :: El
label = el' <<< D.label

label' :: El'
label' = label []

label_ :: El_
label_ = el <<< D.label

label'_ :: El'_
label'_ = label_ []

legend :: El
legend = el' <<< D.legend

legend' :: El'
legend' = legend []

legend_ :: El_
legend_ = el <<< D.legend

legend'_ :: El'_
legend'_ = legend_ []

li :: El
li = el' <<< D.li

li' :: El'
li' = li []

li_ :: El_
li_ = el <<< D.li

li'_ :: El'_
li'_ = li_ []

link :: El
link = el' <<< D.link

link' :: El'
link' = body []

link_ :: El_
link_ = el <<< D.link

link'_ :: El'_
link'_ = body_ []

main :: El
main = el' <<< D.main

main' :: El'
main' = main []

main_ :: El_
main_ = el <<< D.main

main'_ :: El'_
main'_ = main_ []

map :: El
map = el' <<< D.map

map' :: El'
map' = map []

map_ :: El_
map_ = el <<< D.map

map'_ :: El'_
map'_ = map_ []

mark :: El
mark = el' <<< D.mark

mark' :: El'
mark' = mark []

mark_ :: El_
mark_ = el <<< D.mark

mark'_ :: El'_
mark'_ = mark_ []

menu :: El
menu = el' <<< D.menu

menu' :: El'
menu' = menu []

menu_ :: El_
menu_ = el <<< D.menu

menu'_ :: El'_
menu'_ = menu_ []

menuitem :: El
menuitem = el' <<< D.menuitem

menuitem' :: El'
menuitem' = menuitem []

menuitem_ :: El_
menuitem_ = el <<< D.menuitem

menuitem'_ :: El'_
menuitem'_ = menuitem_ []

meta :: El
meta = el' <<< D.meta

meta' :: El'
meta' = meta []

meta_ :: El_
meta_ = el <<< D.meta

meta'_ :: El'_
meta'_ = meta_ []

meter :: El
meter = el' <<< D.meter

meter' :: El'
meter' = meter []

meter_ :: El_
meter_ = el <<< D.meter

meter'_ :: El'_
meter'_ = meter_ []

nav :: El
nav = el' <<< D.nav

nav' :: El'
nav' = nav []

nav_ :: El_
nav_ = el <<< D.nav

nav'_ :: El'_
nav'_ = nav_ []

noscript :: El
noscript = el' <<< D.noscript

noscript' :: El'
noscript' = noscript []

noscript_ :: El_
noscript_ = el <<< D.noscript

noscript'_ :: El'_
noscript'_ = noscript_ []

object :: El
object = el' <<< D.object

object' :: El'
object' = object []

object_ :: El_
object_ = el <<< D.object

object'_ :: El'_
object'_ = object_ []

ol :: El
ol = el' <<< D.ol

ol' :: El'
ol' = ol []

ol_ :: El_
ol_ = el <<< D.ol

ol'_ :: El'_
ol'_ = ol_ []

optgroup :: El
optgroup = el' <<< D.optgroup

optgroup' :: El'
optgroup' = optgroup []

optgroup_ :: El_
optgroup_ = el <<< D.optgroup

optgroup'_ :: El'_
optgroup'_ = optgroup_ []

option :: El
option = el' <<< D.option

option' :: El'
option' = option []

option_ :: El_
option_ = el <<< D.option

option'_ :: El'_
option'_ = option_ []

output :: El
output = el' <<< D.output

output' :: El'
output' = output []

output_ :: El_
output_ = el <<< D.output

output'_ :: El'_
output'_ = output_ []

p :: El
p = el' <<< D.p

p' :: El'
p' = p []

p_ :: El_
p_ = el <<< D.p

p'_ :: El'_
p'_ = p_ []

param :: El
param = el' <<< D.param

param' :: El'
param' = param []

param_ :: El_
param_ = el <<< D.param

param'_ :: El'_
param'_ = param_ []

picture :: El
picture = el' <<< D.picture

picture' :: El'
picture' = picture []

picture_ :: El_
picture_ = el <<< D.picture

picture'_ :: El'_
picture'_ = picture_ []

pre :: El
pre = el' <<< D.pre

pre' :: El'
pre' = pre []

pre_ :: El_
pre_ = el <<< D.pre

pre'_ :: El'_
pre'_ = pre_ []

progress :: El
progress = el' <<< D.progress

progress' :: El'
progress' = progress []

progress_ :: El_
progress_ = el <<< D.progress

progress'_ :: El'_
progress'_ = progress_ []

q :: El
q = el' <<< D.q

q' :: El'
q' = q []

q_ :: El_
q_ = el <<< D.q

q'_ :: El'_
q'_ = q_ []

rp :: El
rp = el' <<< D.rp

rp' :: El'
rp' = rp []

rp_ :: El_
rp_ = el <<< D.rp

rp'_ :: El'_
rp'_ = rp_ []

rt :: El
rt = el' <<< D.rt

rt' :: El'
rt' = rt []

rt_ :: El_
rt_ = el <<< D.rt

rt'_ :: El'_
rt'_ = rt_ []

ruby :: El
ruby = el' <<< D.ruby

ruby' :: El'
ruby' = ruby []

ruby_ :: El_
ruby_ = el <<< D.ruby

ruby'_ :: El'_
ruby'_ = ruby_ []

s :: El
s = el' <<< D.s

s' :: El'
s' = s []

s_ :: El_
s_ = el <<< D.s

s'_ :: El'_
s'_ = s_ []

samp :: El
samp = el' <<< D.samp

samp' :: El'
samp' = samp []

samp_ :: El_
samp_ = el <<< D.samp

samp'_ :: El'_
samp'_ = samp_ []

script :: El
script = el' <<< D.script

script' :: El'
script' = script []

script_ :: El_
script_ = el <<< D.script

script'_ :: El'_
script'_ = script_ []

section :: El
section = el' <<< D.section

section' :: El'
section' = section []

section_ :: El_
section_ = el <<< D.section

section'_ :: El'_
section'_ = section_ []

select :: El
select = el' <<< D.select

select' :: El'
select' = select []

select_ :: El_
select_ = el <<< D.select

select'_ :: El'_
select'_ = select_ []

small :: El
small = el' <<< D.small

small' :: El'
small' = small []

small_ :: El_
small_ = el <<< D.small

small'_ :: El'_
small'_ = small_ []

source :: El
source = el' <<< D.source

source' :: El'
source' = source []

source_ :: El_
source_ = el <<< D.source

source'_ :: El'_
source'_ = source_ []

span :: El
span = el' <<< D.span

span' :: El'
span' = span []

span_ :: El_
span_ = el <<< D.span

span'_ :: El'_
span'_ = span_ []

strong :: El
strong = el' <<< D.strong

strong' :: El'
strong' = strong []

strong_ :: El_
strong_ = el <<< D.strong

strong'_ :: El'_
strong'_ = strong_ []

style :: El
style = el' <<< D.style

style' :: El'
style' = style []

style_ :: El_
style_ = el <<< D.style

style'_ :: El'_
style'_ = style_ []

sub :: El
sub = el' <<< D.sub

sub' :: El'
sub' = sub []

sub_ :: El_
sub_ = el <<< D.sub

sub'_ :: El'_
sub'_ = sub_ []

summary :: El
summary = el' <<< D.summary

summary' :: El'
summary' = summary []

summary_ :: El_
summary_ = el <<< D.summary

summary'_ :: El'_
summary'_ = summary_ []

sup :: El
sup = el' <<< D.sup

sup' :: El'
sup' = sup []

sup_ :: El_
sup_ = el <<< D.sup

sup'_ :: El'_
sup'_ = sup_ []

table :: El
table = el' <<< D.table

table' :: El'
table' = table []

table_ :: El_
table_ = el <<< D.table

table'_ :: El'_
table'_ = table_ []

tbody :: El
tbody = el' <<< D.tbody

tbody' :: El'
tbody' = tbody []

tbody_ :: El_
tbody_ = el <<< D.tbody

tbody'_ :: El'_
tbody'_ = tbody_ []

td :: El
td = el' <<< D.td

td' :: El'
td' = td []

td_ :: El_
td_ = el <<< D.td

td'_ :: El'_
td'_ = td_ []

textarea :: El
textarea = el' <<< D.textarea

textarea' :: El'
textarea' = textarea []

textarea_ :: El_
textarea_ = el <<< D.textarea

textarea'_ :: El'_
textarea'_ = textarea_ []

tfoot :: El
tfoot = el' <<< D.tfoot

tfoot' :: El'
tfoot' = tfoot []

tfoot_ :: El_
tfoot_ = el <<< D.tfoot

tfoot'_ :: El'_
tfoot'_ = tfoot_ []

th :: El
th = el' <<< D.th

th' :: El'
th' = th []

th_ :: El_
th_ = el <<< D.th

th'_ :: El'_
th'_ = th_ []

thead :: El
thead = el' <<< D.thead

thead' :: El'
thead' = thead []

thead_ :: El_
thead_ = el <<< D.thead

thead'_ :: El'_
thead'_ = thead_ []

time :: El
time = el' <<< D.time

time' :: El'
time' = time []

time_ :: El_
time_ = el <<< D.time

time'_ :: El'_
time'_ = time_ []

title :: El
title = el' <<< D.title

title' :: El'
title' = title []

title_ :: El_
title_ = el <<< D.title

title'_ :: El'_
title'_ = title_ []

tr :: El
tr = el' <<< D.tr

tr' :: El'
tr' = tr []

tr_ :: El_
tr_ = el <<< D.tr

tr'_ :: El'_
tr'_ = tr_ []

track :: El
track = el' <<< D.track

track' :: El'
track' = track []

track_ :: El_
track_ = el <<< D.track

track'_ :: El'_
track'_ = track_ []

u :: El
u = el' <<< D.u

u' :: El'
u' = u []

u_ :: El_
u_ = el <<< D.u

u'_ :: El'_
u'_ = u_ []

ul :: El
ul = el' <<< D.ul

ul' :: El'
ul' = ul []

ul_ :: El_
ul_ = el <<< D.ul

ul'_ :: El'_
ul'_ = ul_ []

var :: El
var = el' <<< D.var

var' :: El'
var' = var []

var_ :: El_
var_ = el <<< D.var

var'_ :: El'_
var'_ = var_ []

video :: El
video = el' <<< D.video

video' :: El'
video' = video []

video_ :: El_
video_ = el <<< D.video

video'_ :: El'_
video'_ = video_ []

wbr :: El
wbr = el' <<< D.body

wbr' :: El'
wbr' = wbr []

wbr_ :: El_
wbr_ = el <<< D.body

wbr'_ :: El'_
wbr'_ = wbr_ []
