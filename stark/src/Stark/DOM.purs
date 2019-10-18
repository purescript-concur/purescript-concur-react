module Stark.DOM where

import Stark (StarkElement(..), TagName)
import Stark.DOM.Props (Props)

newtype IsDynamic = IsDynamic Boolean

mkDOM ::
  IsDynamic -> TagName -> Array Props -> Array StarkElement -> StarkElement
mkDOM _ tag props = StarkNode tag props

text :: String -> StarkElement
text = StarkText

int :: Int -> StarkElement
int = StarkInt

number :: Number -> StarkElement
number = StarkNumber

a :: Array Props -> Array StarkElement -> StarkElement
a = mkDOM (IsDynamic false) "a"

a' :: Array StarkElement -> StarkElement
a' = a []

abbr :: Array Props -> Array StarkElement -> StarkElement
abbr = mkDOM (IsDynamic false) "abbr"

abbr' :: Array StarkElement -> StarkElement
abbr' = abbr []

address :: Array Props -> Array StarkElement -> StarkElement
address = mkDOM (IsDynamic false) "address"

address' :: Array StarkElement -> StarkElement
address' = address []

area :: Array Props -> StarkElement
area props = mkDOM (IsDynamic false) "area" props []

area' :: StarkElement
area' = area []

article :: Array Props -> Array StarkElement -> StarkElement
article = mkDOM (IsDynamic false) "article"

article' :: Array StarkElement -> StarkElement
article' = article []

aside :: Array Props -> Array StarkElement -> StarkElement
aside = mkDOM (IsDynamic false) "aside"

aside' :: Array StarkElement -> StarkElement
aside' = aside []

audio :: Array Props -> Array StarkElement -> StarkElement
audio = mkDOM (IsDynamic false) "audio"

audio' :: Array StarkElement -> StarkElement
audio' = audio []

b :: Array Props -> Array StarkElement -> StarkElement
b = mkDOM (IsDynamic false) "b"

b' :: Array StarkElement -> StarkElement
b' = b []

base :: Array Props -> StarkElement
base props = mkDOM (IsDynamic false) "base" props []

base' :: StarkElement
base' = base []

bdi :: Array Props -> Array StarkElement -> StarkElement
bdi = mkDOM (IsDynamic false) "bdi"

bdi' :: Array StarkElement -> StarkElement
bdi' = bdi []

bdo :: Array Props -> Array StarkElement -> StarkElement
bdo = mkDOM (IsDynamic false) "bdo"

bdo' :: Array StarkElement -> StarkElement
bdo' = bdo []

big :: Array Props -> Array StarkElement -> StarkElement
big = mkDOM (IsDynamic false) "big"

big' :: Array StarkElement -> StarkElement
big' = big []

blockquote :: Array Props -> Array StarkElement -> StarkElement
blockquote = mkDOM (IsDynamic false) "blockquote"

blockquote' :: Array StarkElement -> StarkElement
blockquote' = blockquote []

body :: Array Props -> Array StarkElement -> StarkElement
body = mkDOM (IsDynamic false) "body"

body' :: Array StarkElement -> StarkElement
body' = body []

br :: Array Props -> StarkElement
br props = mkDOM (IsDynamic false) "br" props []

br' :: StarkElement
br' = br []

button :: Array Props -> Array StarkElement -> StarkElement
button = mkDOM (IsDynamic false) "button"

button' :: Array StarkElement -> StarkElement
button' = button []

canvas :: Array Props -> Array StarkElement -> StarkElement
canvas = mkDOM (IsDynamic false) "canvas"

canvas' :: Array StarkElement -> StarkElement
canvas' = canvas []

caption :: Array Props -> Array StarkElement -> StarkElement
caption = mkDOM (IsDynamic false) "caption"

caption' :: Array StarkElement -> StarkElement
caption' = caption []

cite :: Array Props -> Array StarkElement -> StarkElement
cite = mkDOM (IsDynamic false) "cite"

cite' :: Array StarkElement -> StarkElement
cite' = cite []

code :: Array Props -> Array StarkElement -> StarkElement
code = mkDOM (IsDynamic false) "code"

code' :: Array StarkElement -> StarkElement
code' = code []

col :: Array Props -> StarkElement
col props = mkDOM (IsDynamic false) "col" props []

col' :: StarkElement
col' = col []

colgroup :: Array Props -> Array StarkElement -> StarkElement
colgroup = mkDOM (IsDynamic false) "colgroup"

colgroup' :: Array StarkElement -> StarkElement
colgroup' = colgroup []

_data :: Array Props -> Array StarkElement -> StarkElement
_data = mkDOM (IsDynamic false) "data"

_data' :: Array StarkElement -> StarkElement
_data' = _data []

datalist :: Array Props -> Array StarkElement -> StarkElement
datalist = mkDOM (IsDynamic false) "datalist"

datalist' :: Array StarkElement -> StarkElement
datalist' = datalist []

dd :: Array Props -> Array StarkElement -> StarkElement
dd = mkDOM (IsDynamic false) "dd"

dd' :: Array StarkElement -> StarkElement
dd' = dd []

del :: Array Props -> Array StarkElement -> StarkElement
del = mkDOM (IsDynamic false) "del"

del' :: Array StarkElement -> StarkElement
del' = del []

details :: Array Props -> Array StarkElement -> StarkElement
details = mkDOM (IsDynamic false) "details"

details' :: Array StarkElement -> StarkElement
details' = details []

dfn :: Array Props -> Array StarkElement -> StarkElement
dfn = mkDOM (IsDynamic false) "dfn"

dfn' :: Array StarkElement -> StarkElement
dfn' = dfn []

dialog :: Array Props -> Array StarkElement -> StarkElement
dialog = mkDOM (IsDynamic false) "dialog"

dialog' :: Array StarkElement -> StarkElement
dialog' = dialog []

div :: Array Props -> Array StarkElement -> StarkElement
div = mkDOM (IsDynamic false) "div"

div' :: Array StarkElement -> StarkElement
div' = div []

dl :: Array Props -> Array StarkElement -> StarkElement
dl = mkDOM (IsDynamic false) "dl"

dl' :: Array StarkElement -> StarkElement
dl' = dl []

dt :: Array Props -> Array StarkElement -> StarkElement
dt = mkDOM (IsDynamic false) "dt"

dt' :: Array StarkElement -> StarkElement
dt' = dt []

em :: Array Props -> Array StarkElement -> StarkElement
em = mkDOM (IsDynamic false) "em"

em' :: Array StarkElement -> StarkElement
em' = em []

embed :: Array Props -> StarkElement
embed props = mkDOM (IsDynamic false) "embed" props []

embed' :: StarkElement
embed' = embed []

fieldset :: Array Props -> Array StarkElement -> StarkElement
fieldset = mkDOM (IsDynamic false) "fieldset"

fieldset' :: Array StarkElement -> StarkElement
fieldset' = fieldset []

figcaption :: Array Props -> Array StarkElement -> StarkElement
figcaption = mkDOM (IsDynamic false) "figcaption"

figcaption' :: Array StarkElement -> StarkElement
figcaption' = figcaption []

figure :: Array Props -> Array StarkElement -> StarkElement
figure = mkDOM (IsDynamic false) "figure"

figure' :: Array StarkElement -> StarkElement
figure' = figure []

footer :: Array Props -> Array StarkElement -> StarkElement
footer = mkDOM (IsDynamic false) "footer"

footer' :: Array StarkElement -> StarkElement
footer' = footer []

form :: Array Props -> Array StarkElement -> StarkElement
form = mkDOM (IsDynamic false) "form"

form' :: Array StarkElement -> StarkElement
form' = form []

h1 :: Array Props -> Array StarkElement -> StarkElement
h1 = mkDOM (IsDynamic false) "h1"

h1' :: Array StarkElement -> StarkElement
h1' = h1 []

h2 :: Array Props -> Array StarkElement -> StarkElement
h2 = mkDOM (IsDynamic false) "h2"

h2' :: Array StarkElement -> StarkElement
h2' = h2 []

h3 :: Array Props -> Array StarkElement -> StarkElement
h3 = mkDOM (IsDynamic false) "h3"

h3' :: Array StarkElement -> StarkElement
h3' = h3 []

h4 :: Array Props -> Array StarkElement -> StarkElement
h4 = mkDOM (IsDynamic false) "h4"

h4' :: Array StarkElement -> StarkElement
h4' = h4 []

h5 :: Array Props -> Array StarkElement -> StarkElement
h5 = mkDOM (IsDynamic false) "h5"

h5' :: Array StarkElement -> StarkElement
h5' = h5 []

h6 :: Array Props -> Array StarkElement -> StarkElement
h6 = mkDOM (IsDynamic false) "h6"

h6' :: Array StarkElement -> StarkElement
h6' = h6 []

head :: Array Props -> Array StarkElement -> StarkElement
head = mkDOM (IsDynamic false) "head"

head' :: Array StarkElement -> StarkElement
head' = head []

header :: Array Props -> Array StarkElement -> StarkElement
header = mkDOM (IsDynamic false) "header"

header' :: Array StarkElement -> StarkElement
header' = header []

hr :: Array Props -> StarkElement
hr props = mkDOM (IsDynamic false) "hr" props []

hr' :: StarkElement
hr' = hr []

html :: Array Props -> Array StarkElement -> StarkElement
html = mkDOM (IsDynamic false) "html"

html' :: Array StarkElement -> StarkElement
html' = html []

i :: Array Props -> Array StarkElement -> StarkElement
i = mkDOM (IsDynamic false) "i"

i' :: Array StarkElement -> StarkElement
i' = i []

iframe :: Array Props -> Array StarkElement -> StarkElement
iframe = mkDOM (IsDynamic false) "iframe"

iframe' :: Array StarkElement -> StarkElement
iframe' = iframe []

img :: Array Props -> StarkElement
img props = mkDOM (IsDynamic false) "img" props []

img' :: StarkElement
img' = img []

input :: Array Props -> StarkElement
input props = mkDOM (IsDynamic false) "input" props []

input' :: StarkElement
input' = input []

ins :: Array Props -> Array StarkElement -> StarkElement
ins = mkDOM (IsDynamic false) "ins"

ins' :: Array StarkElement -> StarkElement
ins' = ins []

kbd :: Array Props -> Array StarkElement -> StarkElement
kbd = mkDOM (IsDynamic false) "kbd"

kbd' :: Array StarkElement -> StarkElement
kbd' = kbd []

keygen :: Array Props -> StarkElement
keygen props = mkDOM (IsDynamic false) "keygen" props []

keygen' :: StarkElement
keygen' = keygen []

label :: Array Props -> Array StarkElement -> StarkElement
label = mkDOM (IsDynamic false) "label"

label' :: Array StarkElement -> StarkElement
label' = label []

legend :: Array Props -> Array StarkElement -> StarkElement
legend = mkDOM (IsDynamic false) "legend"

legend' :: Array StarkElement -> StarkElement
legend' = legend []

li :: Array Props -> Array StarkElement -> StarkElement
li = mkDOM (IsDynamic false) "li"

li' :: Array StarkElement -> StarkElement
li' = li []

link :: Array Props -> StarkElement
link props = mkDOM (IsDynamic false) "link" props []

link' :: StarkElement
link' = link []

main :: Array Props -> Array StarkElement -> StarkElement
main = mkDOM (IsDynamic false) "main"

main' :: Array StarkElement -> StarkElement
main' = main []

map :: Array Props -> Array StarkElement -> StarkElement
map = mkDOM (IsDynamic false) "map"

map' :: Array StarkElement -> StarkElement
map' = map []

mark :: Array Props -> Array StarkElement -> StarkElement
mark = mkDOM (IsDynamic false) "mark"

mark' :: Array StarkElement -> StarkElement
mark' = mark []

menu :: Array Props -> Array StarkElement -> StarkElement
menu = mkDOM (IsDynamic false) "menu"

menu' :: Array StarkElement -> StarkElement
menu' = menu []

menuitem :: Array Props -> StarkElement
menuitem props = mkDOM (IsDynamic false) "menuitem" props []

menuitem' :: StarkElement
menuitem' = menuitem []

meta :: Array Props -> StarkElement
meta props = mkDOM (IsDynamic false) "meta" props []

meta' :: StarkElement
meta' = meta []

meter :: Array Props -> Array StarkElement -> StarkElement
meter = mkDOM (IsDynamic false) "meter"

meter' :: Array StarkElement -> StarkElement
meter' = meter []

nav :: Array Props -> Array StarkElement -> StarkElement
nav = mkDOM (IsDynamic false) "nav"

nav' :: Array StarkElement -> StarkElement
nav' = nav []

noscript :: Array Props -> Array StarkElement -> StarkElement
noscript = mkDOM (IsDynamic false) "noscript"

noscript' :: Array StarkElement -> StarkElement
noscript' = noscript []

object :: Array Props -> Array StarkElement -> StarkElement
object = mkDOM (IsDynamic false) "object"

object' :: Array StarkElement -> StarkElement
object' = object []

ol :: Array Props -> Array StarkElement -> StarkElement
ol = mkDOM (IsDynamic false) "ol"

ol' :: Array StarkElement -> StarkElement
ol' = ol []

optgroup :: Array Props -> Array StarkElement -> StarkElement
optgroup = mkDOM (IsDynamic false) "optgroup"

optgroup' :: Array StarkElement -> StarkElement
optgroup' = optgroup []

option :: Array Props -> Array StarkElement -> StarkElement
option = mkDOM (IsDynamic false) "option"

option' :: Array StarkElement -> StarkElement
option' = option []

output :: Array Props -> Array StarkElement -> StarkElement
output = mkDOM (IsDynamic false) "output"

output' :: Array StarkElement -> StarkElement
output' = output []

p :: Array Props -> Array StarkElement -> StarkElement
p = mkDOM (IsDynamic false) "p"

p' :: Array StarkElement -> StarkElement
p' = p []

param :: Array Props -> StarkElement
param props = mkDOM (IsDynamic false) "param" props []

param' :: StarkElement
param' = param []

picture :: Array Props -> Array StarkElement -> StarkElement
picture = mkDOM (IsDynamic false) "picture"

picture' :: Array StarkElement -> StarkElement
picture' = picture []

pre :: Array Props -> Array StarkElement -> StarkElement
pre = mkDOM (IsDynamic false) "pre"

pre' :: Array StarkElement -> StarkElement
pre' = pre []

progress :: Array Props -> Array StarkElement -> StarkElement
progress = mkDOM (IsDynamic false) "progress"

progress' :: Array StarkElement -> StarkElement
progress' = progress []

q :: Array Props -> Array StarkElement -> StarkElement
q = mkDOM (IsDynamic false) "q"

q' :: Array StarkElement -> StarkElement
q' = q []

rp :: Array Props -> Array StarkElement -> StarkElement
rp = mkDOM (IsDynamic false) "rp"

rp' :: Array StarkElement -> StarkElement
rp' = rp []

rt :: Array Props -> Array StarkElement -> StarkElement
rt = mkDOM (IsDynamic false) "rt"

rt' :: Array StarkElement -> StarkElement
rt' = rt []

ruby :: Array Props -> Array StarkElement -> StarkElement
ruby = mkDOM (IsDynamic false) "ruby"

ruby' :: Array StarkElement -> StarkElement
ruby' = ruby []

s :: Array Props -> Array StarkElement -> StarkElement
s = mkDOM (IsDynamic false) "s"

s' :: Array StarkElement -> StarkElement
s' = s []

samp :: Array Props -> Array StarkElement -> StarkElement
samp = mkDOM (IsDynamic false) "samp"

samp' :: Array StarkElement -> StarkElement
samp' = samp []

script :: Array Props -> Array StarkElement -> StarkElement
script = mkDOM (IsDynamic false) "script"

script' :: Array StarkElement -> StarkElement
script' = script []

section :: Array Props -> Array StarkElement -> StarkElement
section = mkDOM (IsDynamic false) "section"

section' :: Array StarkElement -> StarkElement
section' = section []

select :: Array Props -> Array StarkElement -> StarkElement
select = mkDOM (IsDynamic false) "select"

select' :: Array StarkElement -> StarkElement
select' = select []

small :: Array Props -> Array StarkElement -> StarkElement
small = mkDOM (IsDynamic false) "small"

small' :: Array StarkElement -> StarkElement
small' = small []

source :: Array Props -> StarkElement
source props = mkDOM (IsDynamic false) "source" props []

source' :: StarkElement
source' = source []

span :: Array Props -> Array StarkElement -> StarkElement
span = mkDOM (IsDynamic false) "span"

span' :: Array StarkElement -> StarkElement
span' = span []

strong :: Array Props -> Array StarkElement -> StarkElement
strong = mkDOM (IsDynamic false) "strong"

strong' :: Array StarkElement -> StarkElement
strong' = strong []

style :: Array Props -> Array StarkElement -> StarkElement
style = mkDOM (IsDynamic false) "style"

style' :: Array StarkElement -> StarkElement
style' = style []

sub :: Array Props -> Array StarkElement -> StarkElement
sub = mkDOM (IsDynamic false) "sub"

sub' :: Array StarkElement -> StarkElement
sub' = sub []

summary :: Array Props -> Array StarkElement -> StarkElement
summary = mkDOM (IsDynamic false) "summary"

summary' :: Array StarkElement -> StarkElement
summary' = summary []

sup :: Array Props -> Array StarkElement -> StarkElement
sup = mkDOM (IsDynamic false) "sup"

sup' :: Array StarkElement -> StarkElement
sup' = sup []

table :: Array Props -> Array StarkElement -> StarkElement
table = mkDOM (IsDynamic false) "table"

table' :: Array StarkElement -> StarkElement
table' = table []

tbody :: Array Props -> Array StarkElement -> StarkElement
tbody = mkDOM (IsDynamic false) "tbody"

tbody' :: Array StarkElement -> StarkElement
tbody' = tbody []

td :: Array Props -> Array StarkElement -> StarkElement
td = mkDOM (IsDynamic false) "td"

td' :: Array StarkElement -> StarkElement
td' = td []

textarea :: Array Props -> Array StarkElement -> StarkElement
textarea = mkDOM (IsDynamic false) "textarea"

textarea' :: Array StarkElement -> StarkElement
textarea' = textarea []

tfoot :: Array Props -> Array StarkElement -> StarkElement
tfoot = mkDOM (IsDynamic false) "tfoot"

tfoot' :: Array StarkElement -> StarkElement
tfoot' = tfoot []

th :: Array Props -> Array StarkElement -> StarkElement
th = mkDOM (IsDynamic false) "th"

th' :: Array StarkElement -> StarkElement
th' = th []

thead :: Array Props -> Array StarkElement -> StarkElement
thead = mkDOM (IsDynamic false) "thead"

thead' :: Array StarkElement -> StarkElement
thead' = thead []

time :: Array Props -> Array StarkElement -> StarkElement
time = mkDOM (IsDynamic false) "time"

time' :: Array StarkElement -> StarkElement
time' = time []

title :: Array Props -> Array StarkElement -> StarkElement
title = mkDOM (IsDynamic false) "title"

title' :: Array StarkElement -> StarkElement
title' = title []

tr :: Array Props -> Array StarkElement -> StarkElement
tr = mkDOM (IsDynamic false) "tr"

tr' :: Array StarkElement -> StarkElement
tr' = tr []

track :: Array Props -> StarkElement
track props = mkDOM (IsDynamic false) "track" props []

track' :: StarkElement
track' = track []

u :: Array Props -> Array StarkElement -> StarkElement
u = mkDOM (IsDynamic false) "u"

u' :: Array StarkElement -> StarkElement
u' = u []

ul :: Array Props -> Array StarkElement -> StarkElement
ul = mkDOM (IsDynamic false) "ul"

ul' :: Array StarkElement -> StarkElement
ul' = ul []

var :: Array Props -> Array StarkElement -> StarkElement
var = mkDOM (IsDynamic false) "var"

var' :: Array StarkElement -> StarkElement
var' = var []

video :: Array Props -> Array StarkElement -> StarkElement
video = mkDOM (IsDynamic false) "video"

video' :: Array StarkElement -> StarkElement
video' = video []

wbr :: Array Props -> StarkElement
wbr props = mkDOM (IsDynamic false) "wbr" props []

wbr' :: StarkElement
wbr' = wbr []
