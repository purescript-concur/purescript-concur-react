module Concur.React.Props where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IOSync (runIOSync', IOSync)
import Data.Array (concatMap, intercalate)
import Data.Maybe (Maybe(..), maybe)
import React (Event, KeyboardEvent, MouseEvent, handle)
import React.DOM.Props as P

data Props a = PrimProp P.Props | Handler ((a -> IOSync Unit) -> P.Props)

instance functorProps :: Functor Props where
  map _ (PrimProp p) = PrimProp p
  map f (Handler h) = Handler \k -> h (k <<< f)

-- | Internal. Do not use. Use unsafeMkProp, or unsafeMkPropHandler instead.
mkProp :: forall a. (a -> IOSync Unit) -> Props a -> P.Props
mkProp _ (PrimProp a) = a
mkProp h (Handler f) = f h

-- | Construct a custom prop handler
unsafeMkPropHandler :: forall a. String -> Props a
unsafeMkPropHandler s = Handler \f -> P.unsafeMkProps s (handle (\e -> runIOSync' (f e)))

-- | Construct a custom key value prop
unsafeMkProp :: forall a. String -> a -> Props a
unsafeMkProp s v = PrimProp (P.unsafeMkProps s v)

-- | Shortcut for the common case of a list of classes
classList :: forall a. Array (Maybe String) -> Props a
classList = className <<< intercalate " " <<< concatMap (maybe [] (\s -> [s]))

-- The standard set of props

aria :: forall ariaAttrs a. { | ariaAttrs } -> Props a
aria = PrimProp <<< P.aria

_data :: forall dataAttrs a. { | dataAttrs } -> Props a
_data = PrimProp <<< P._data

style :: forall style a. { | style } -> Props a
style = PrimProp <<< P.style

dangerouslySetInnerHTML :: forall a. { __html :: String } -> Props a
dangerouslySetInnerHTML = PrimProp <<< P.dangerouslySetInnerHTML

accept :: forall a. String -> Props a
accept = PrimProp <<< P.accept

acceptCharset :: forall a. String -> Props a
acceptCharset = PrimProp <<< P.acceptCharset

accessKey :: forall a. String -> Props a
accessKey = PrimProp <<< P.accessKey

action :: forall a. String -> Props a
action = PrimProp <<< P.action

allowFullScreen :: forall a. Boolean -> Props a
allowFullScreen = PrimProp <<< P.allowFullScreen

allowTransparency :: forall a. Boolean -> Props a
allowTransparency = PrimProp <<< P.allowTransparency

alt :: forall a. String -> Props a
alt = PrimProp <<< P.alt

async :: forall a. Boolean -> Props a
async = PrimProp <<< P.async

autoComplete :: forall a. String -> Props a
autoComplete = PrimProp <<< P.autoComplete

autoFocus :: forall a. Boolean -> Props a
autoFocus = PrimProp <<< P.autoFocus

autoPlay :: forall a. Boolean -> Props a
autoPlay = PrimProp <<< P.autoPlay

capture :: forall a. Boolean -> Props a
capture = PrimProp <<< P.capture

cellPadding :: forall a. String -> Props a
cellPadding = PrimProp <<< P.cellPadding

cellSpacing :: forall a. String -> Props a
cellSpacing = PrimProp <<< P.cellSpacing

charSet :: forall a. String -> Props a
charSet = PrimProp <<< P.charSet

challenge :: forall a. String -> Props a
challenge = PrimProp <<< P.challenge

checked :: forall a. Boolean -> Props a
checked = PrimProp <<< P.checked

cite :: forall a. String -> Props a
cite = PrimProp <<< P.cite

classID :: forall a. String -> Props a
classID = PrimProp <<< P.classID

className :: forall a. String -> Props a
className = PrimProp <<< P.className

cols :: forall a. Int -> Props a
cols = PrimProp <<< P.cols

colSpan :: forall a. Int -> Props a
colSpan = PrimProp <<< P.colSpan

content :: forall a. String -> Props a
content = PrimProp <<< P.content

contentEditable :: forall a. Boolean -> Props a
contentEditable = PrimProp <<< P.contentEditable

contextMenu :: forall a. String -> Props a
contextMenu = PrimProp <<< P.contextMenu

controls :: forall a. Boolean -> Props a
controls = PrimProp <<< P.controls

coords :: forall a. String -> Props a
coords = PrimProp <<< P.coords

crossOrigin :: forall a. String -> Props a
crossOrigin = PrimProp <<< P.crossOrigin

dateTime :: forall a. String -> Props a
dateTime = PrimProp <<< P.dateTime

default :: forall a. Boolean -> Props a
default = PrimProp <<< P.default

defaultChecked :: forall a. Boolean -> Props a
defaultChecked = PrimProp <<< P.defaultChecked

defaultValue :: forall a. String -> Props a
defaultValue = PrimProp <<< P.defaultValue

defer :: forall a. Boolean -> Props a
defer = PrimProp <<< P.defer

dir :: forall a. String -> Props a
dir = PrimProp <<< P.dir

disabled :: forall a. Boolean -> Props a
disabled = PrimProp <<< P.disabled

download :: forall a. String -> Props a
download = PrimProp <<< P.download

draggable :: forall a. Boolean -> Props a
draggable = PrimProp <<< P.draggable

encType :: forall a. String -> Props a
encType = PrimProp <<< P.encType

form :: forall a. String -> Props a
form = PrimProp <<< P.form

formAction :: forall a. String -> Props a
formAction = PrimProp <<< P.formAction

formEncType :: forall a. String -> Props a
formEncType = PrimProp <<< P.formEncType

formMethod :: forall a. String -> Props a
formMethod = PrimProp <<< P.formMethod

formNoValidate :: forall a. Boolean -> Props a
formNoValidate = PrimProp <<< P.formNoValidate

formTarget :: forall a. String -> Props a
formTarget = PrimProp <<< P.formTarget

frameBorder :: forall a. String -> Props a
frameBorder = PrimProp <<< P.frameBorder

headers :: forall a. String -> Props a
headers = PrimProp <<< P.headers

height :: forall a. String -> Props a
height = PrimProp <<< P.height

hidden :: forall a. Boolean -> Props a
hidden = PrimProp <<< P.hidden

high :: forall a. String -> Props a
high = PrimProp <<< P.high

href :: forall a. String -> Props a
href = PrimProp <<< P.href

hrefLang :: forall a. String -> Props a
hrefLang = PrimProp <<< P.hrefLang

htmlFor :: forall a. String -> Props a
htmlFor = PrimProp <<< P.htmlFor

httpEquiv :: forall a. String -> Props a
httpEquiv = PrimProp <<< P.httpEquiv

icon :: forall a. String -> Props a
icon = PrimProp <<< P.icon

_id :: forall a. String -> Props a
_id = PrimProp <<< P._id

inputMode :: forall a. String -> Props a
inputMode = PrimProp <<< P.inputMode

integrity :: forall a. String -> Props a
integrity = PrimProp <<< P.integrity

is :: forall a. String -> Props a
is = PrimProp <<< P.is

key :: forall a. String -> Props a
key = PrimProp <<< P.key

keyParams :: forall a. String -> Props a
keyParams = PrimProp <<< P.keyParams

keyType :: forall a. String -> Props a
keyType = PrimProp <<< P.keyType

kind :: forall a. String -> Props a
kind = PrimProp <<< P.kind

label :: forall a. String -> Props a
label = PrimProp <<< P.label

lang :: forall a. String -> Props a
lang = PrimProp <<< P.lang

list :: forall a. String -> Props a
list = PrimProp <<< P.list

loop :: forall a. Boolean -> Props a
loop = PrimProp <<< P.loop

low :: forall a. String -> Props a
low = PrimProp <<< P.low

manifest :: forall a. String -> Props a
manifest = PrimProp <<< P.manifest

marginHeight :: forall a. String -> Props a
marginHeight = PrimProp <<< P.marginHeight

marginWidth :: forall a. String -> Props a
marginWidth = PrimProp <<< P.marginWidth

max :: forall a. String -> Props a
max = PrimProp <<< P.max

maxLength :: forall a. String -> Props a
maxLength = PrimProp <<< P.maxLength

media :: forall a. String -> Props a
media = PrimProp <<< P.media

mediaGroup :: forall a. String -> Props a
mediaGroup = PrimProp <<< P.mediaGroup

method :: forall a. String -> Props a
method = PrimProp <<< P.method

min :: forall a. String -> Props a
min = PrimProp <<< P.min

minLength :: forall a. String -> Props a
minLength = PrimProp <<< P.minLength

multiple :: forall a. Boolean -> Props a
multiple = PrimProp <<< P.multiple

muted :: forall a. Boolean -> Props a
muted = PrimProp <<< P.muted

name :: forall a. String -> Props a
name = PrimProp <<< P.name

nonce :: forall a. String -> Props a
nonce = PrimProp <<< P.nonce

noValidate :: forall a. Boolean -> Props a
noValidate = PrimProp <<< P.noValidate

open :: forall a. Boolean -> Props a
open = PrimProp <<< P.open

optimum :: forall a. String -> Props a
optimum = PrimProp <<< P.optimum

pattern :: forall a. String -> Props a
pattern = PrimProp <<< P.pattern

placeholder :: forall a. String -> Props a
placeholder = PrimProp <<< P.placeholder

poster :: forall a. String -> Props a
poster = PrimProp <<< P.poster

preload :: forall a. String -> Props a
preload = PrimProp <<< P.preload

profile :: forall a. String -> Props a
profile = PrimProp <<< P.profile

radioGroup :: forall a. String -> Props a
radioGroup = PrimProp <<< P.radioGroup

readOnly :: forall a. Boolean -> Props a
readOnly = PrimProp <<< P.readOnly

rel :: forall a. String -> Props a
rel = PrimProp <<< P.rel

required :: forall a. Boolean -> Props a
required = PrimProp <<< P.required

reversed :: forall a. Boolean -> Props a
reversed = PrimProp <<< P.reversed

role :: forall a. String -> Props a
role = PrimProp <<< P.role

rows :: forall a. Int -> Props a
rows = PrimProp <<< P.rows

rowSpan :: forall a. Int -> Props a
rowSpan = PrimProp <<< P.rowSpan

sandbox :: forall a. String -> Props a
sandbox = PrimProp <<< P.sandbox

scope :: forall a. String -> Props a
scope = PrimProp <<< P.scope

scoped :: forall a. Boolean -> Props a
scoped = PrimProp <<< P.scoped

scrolling :: forall a. String -> Props a
scrolling = PrimProp <<< P.scrolling

seamless :: forall a. Boolean -> Props a
seamless = PrimProp <<< P.seamless

selected :: forall a. Boolean -> Props a
selected = PrimProp <<< P.selected

shape :: forall a. String -> Props a
shape = PrimProp <<< P.shape

size :: forall a. Int -> Props a
size = PrimProp <<< P.size

sizes :: forall a. String -> Props a
sizes = PrimProp <<< P.sizes

span :: forall a. Int -> Props a
span = PrimProp <<< P.span

spellCheck :: forall a. Boolean -> Props a
spellCheck = PrimProp <<< P.spellCheck

src :: forall a. String -> Props a
src = PrimProp <<< P.src

srcDoc :: forall a. String -> Props a
srcDoc = PrimProp <<< P.srcDoc

srcLang :: forall a. String -> Props a
srcLang = PrimProp <<< P.srcLang

srcSet :: forall a. String -> Props a
srcSet = PrimProp <<< P.srcSet

start :: forall a. Int -> Props a
start = PrimProp <<< P.start

step :: forall a. String -> Props a
step = PrimProp <<< P.step

summary :: forall a. String -> Props a
summary = PrimProp <<< P.summary

tabIndex :: forall a. Int -> Props a
tabIndex = PrimProp <<< P.tabIndex

target :: forall a. String -> Props a
target = PrimProp <<< P.target

title :: forall a. String -> Props a
title = PrimProp <<< P.title

_type :: forall a. String -> Props a
_type = PrimProp <<< P._type

useMap :: forall a. String -> Props a
useMap = PrimProp <<< P.useMap

value :: forall a. String -> Props a
value = PrimProp <<< P.value

valueArray :: forall a. Array String -> Props a
valueArray = PrimProp <<< P.valueArray

width :: forall a. String -> Props a
width = PrimProp <<< P.width

wmode :: forall a. String -> Props a
wmode = PrimProp <<< P.wmode

wrap :: forall a. String -> Props a
wrap = PrimProp <<< P.wrap

-- RDFa Attributes
about :: forall a. String -> Props a
about = PrimProp <<< P.about

datatype :: forall a. String -> Props a
datatype = PrimProp <<< P.datatype

inlist :: forall a. String -> Props a
inlist = PrimProp <<< P.inlist

prefix :: forall a. String -> Props a
prefix = PrimProp <<< P.prefix

property :: forall a. String -> Props a
property = PrimProp <<< P.property

resource :: forall a. String -> Props a
resource = PrimProp <<< P.resource

typeof :: forall a. String -> Props a
typeof = PrimProp <<< P.typeof

vocab :: forall a. String -> Props a
vocab = PrimProp <<< P.vocab

-- Non-standard Attributes
autoCapitalize :: forall a. String -> Props a
autoCapitalize = PrimProp <<< P.autoCapitalize

autoCorrect :: forall a. String -> Props a
autoCorrect = PrimProp <<< P.autoCorrect

autoSave :: forall a. String -> Props a
autoSave = PrimProp <<< P.autoSave

color :: forall a. String -> Props a
color = PrimProp <<< P.color

itemProp :: forall a. String -> Props a
itemProp = PrimProp <<< P.itemProp

itemScope :: forall a. Boolean -> Props a
itemScope = PrimProp <<< P.itemScope

itemType :: forall a. String -> Props a
itemType = PrimProp <<< P.itemType

itemID :: forall a. String -> Props a
itemID = PrimProp <<< P.itemID

itemRef :: forall a. String -> Props a
itemRef = PrimProp <<< P.itemRef

results :: forall a. Int -> Props a
results = PrimProp <<< P.results

security :: forall a. String -> Props a
security = PrimProp <<< P.security

unselectable :: forall a. Boolean -> Props a
unselectable = PrimProp <<< P.unselectable

-- | Event producing props.
-- | React's event objects are reused ("for efficiency") and thus are not usable outside
-- | of the immediate event handler. This means that a prop that returns a React Event is not useful by itself.
-- | If you want to fetch some properties from the event object, you can use one of the appropriate methods in this module.

onAnimationStart :: Props Event
onAnimationStart = Handler \k -> P.onAnimationStart (runIOSync' <<< k)

onAnimationEnd :: Props Event
onAnimationEnd = Handler \k -> P.onAnimationEnd (runIOSync' <<< k)

onAnimationIteration :: Props Event
onAnimationIteration = Handler \k -> P.onAnimationIteration (runIOSync' <<< k)

onTransitionEnd :: Props Event
onTransitionEnd = Handler \k -> P.onTransitionEnd (runIOSync' <<< k)

onLoad :: Props Event
onLoad = Handler \k -> P.onLoad (runIOSync' <<< k)

onCopy :: Props Event
onCopy = Handler \k -> P.onCopy (runIOSync' <<< k)

onCut :: Props Event
onCut = Handler \k -> P.onCut (runIOSync' <<< k)

onPaste :: Props Event
onPaste = Handler \k -> P.onPaste (runIOSync' <<< k)

onKeyDown :: Props KeyboardEvent
onKeyDown = Handler \k -> P.onKeyDown (runIOSync' <<< k)

onKeyPress :: Props KeyboardEvent
onKeyPress = Handler \k -> P.onKeyPress (runIOSync' <<< k)

onKeyUp :: Props KeyboardEvent
onKeyUp = Handler \k -> P.onKeyUp (runIOSync' <<< k)

onFocus :: Props Event
onFocus = Handler \k -> P.onFocus (runIOSync' <<< k)

onBlur :: Props Event
onBlur = Handler \k -> P.onBlur (runIOSync' <<< k)

onChange :: Props Event
onChange = Handler \k -> P.onChange (runIOSync' <<< k)

onInput :: Props Event
onInput = Handler \k -> P.onInput (runIOSync' <<< k)

onInvalid :: Props Event
onInvalid = Handler \k -> P.onInvalid (runIOSync' <<< k)

onSubmit :: Props Event
onSubmit = Handler \k -> P.onSubmit (runIOSync' <<< k)

onClick :: Props Event
onClick = Handler \k -> P.onClick (runIOSync' <<< k)

onDoubleClick :: Props Event
onDoubleClick = Handler \k -> P.onDoubleClick (runIOSync' <<< k)

onDrag :: Props Event
onDrag = Handler \k -> P.onDrag (runIOSync' <<< k)

onDragEnd :: Props Event
onDragEnd = Handler \k -> P.onDragEnd (runIOSync' <<< k)

onDragEnter :: Props Event
onDragEnter = Handler \k -> P.onDragEnter (runIOSync' <<< k)

onDragExit :: Props Event
onDragExit = Handler \k -> P.onDragExit (runIOSync' <<< k)

onDragLeave :: Props Event
onDragLeave = Handler \k -> P.onDragLeave (runIOSync' <<< k)

onDragOver :: Props Event
onDragOver = Handler \k -> P.onDragOver (runIOSync' <<< k)

onDragStart :: Props Event
onDragStart = Handler \k -> P.onDragStart (runIOSync' <<< k)

onDrop :: Props Event
onDrop = Handler \k -> P.onDrop (runIOSync' <<< k)

onMouseDown :: Props MouseEvent
onMouseDown = Handler \k -> P.onMouseDown (runIOSync' <<< k)

onMouseEnter :: Props MouseEvent
onMouseEnter = Handler \k -> P.onMouseEnter (runIOSync' <<< k)

onMouseLeave :: Props MouseEvent
onMouseLeave = Handler \k -> P.onMouseLeave (runIOSync' <<< k)

onMouseMove :: Props MouseEvent
onMouseMove = Handler \k -> P.onMouseMove (runIOSync' <<< k)

onMouseOut :: Props MouseEvent
onMouseOut = Handler \k -> P.onMouseOut (runIOSync' <<< k)

onMouseOver :: Props MouseEvent
onMouseOver = Handler \k -> P.onMouseOver (runIOSync' <<< k)

onMouseUp :: Props MouseEvent
onMouseUp = Handler \k -> P.onMouseUp (runIOSync' <<< k)

onTouchCancel :: Props Event
onTouchCancel = Handler \k -> P.onTouchCancel (runIOSync' <<< k)

onTouchEnd :: Props Event
onTouchEnd = Handler \k -> P.onTouchEnd (runIOSync' <<< k)

onTouchMove :: Props Event
onTouchMove = Handler \k -> P.onTouchMove (runIOSync' <<< k)

onTouchStart :: Props Event
onTouchStart = Handler \k -> P.onTouchStart (runIOSync' <<< k)

onScroll :: Props Event
onScroll = Handler \k -> P.onScroll (runIOSync' <<< k)

onWheel :: Props Event
onWheel = Handler \k -> P.onWheel (runIOSync' <<< k)

suppressContentEditableWarning :: forall a. Boolean -> Props a
suppressContentEditableWarning = PrimProp <<< P.suppressContentEditableWarning

-- SVG attributes
x :: forall a. Int -> Props a
x = PrimProp <<< P.x

y :: forall a. Int -> Props a
y = PrimProp <<< P.y

cx :: forall a. Int -> Props a
cx = PrimProp <<< P.cx

cy :: forall a. Int -> Props a
cy = PrimProp <<< P.cy

r :: forall a. Int -> Props a
r = PrimProp <<< P.r

fill :: forall a. String -> Props a
fill = PrimProp <<< P.fill

opacity :: forall a. Int -> Props a
opacity = PrimProp <<< P.opacity

fillOpacity :: forall a. Int -> Props a
fillOpacity = PrimProp <<< P.fillOpacity

stroke :: forall a. String -> Props a
stroke = PrimProp <<< P.stroke

strokeWidth :: forall a. Int -> Props a
strokeWidth = PrimProp <<< P.strokeWidth

points :: forall a. String -> Props a
points = PrimProp <<< P.points

d :: forall a. String -> Props a
d = PrimProp <<< P.d

viewBox :: forall a. String -> Props a
viewBox = PrimProp <<< P.viewBox

--------------------------------------------------------------------------------
-- Event props

-- | Converts a prop to return the input target value instead of the event object
inputValue :: Props Event -> Props String
inputValue = mapMaybe (\e -> Just <$> liftEff (getEventTargetValueString e))

-- | Selectively invoke event handlers
-- | The prop does not return until the mapped function returns (Just val)
mapMaybe :: forall a b. Show b => (a -> IOSync (Maybe b)) -> Props a -> Props b
mapMaybe _ (PrimProp p) = PrimProp p
mapMaybe f (Handler h) = Handler \k -> h (g k)
  where
    g k a = do
      b' <- f a
      case b' of
        Just b -> k b
        Nothing -> pure unit

-- | Special custom prop that only returns when the enter key is pressed.
-- Can't use Concur.React.Props.onKeyDown because that doesn't allow getting event information (i.e. value) in the handler
-- Also have to manually handle resetting the value of the input on enter.
onHandleEnter :: Props String
onHandleEnter = mapMaybe g (unsafeMkPropHandler "onKeyDown")
  where
    g e = if (getKeyboardEventKeyString e == "Enter")
      then do
        liftEff (resetTargetValue "" e)
        Just <$> liftEff (getEventTargetValueString e)
      else pure Nothing

-- Generic function to get info out of events
-- TODO: Move these to some other place
foreign import getKeyboardEventKeyString :: Event -> String
foreign import getEventTargetValueString :: Event -> forall eff. Eff eff String
foreign import resetTargetValue :: forall eff. String -> Event -> Eff eff Unit
