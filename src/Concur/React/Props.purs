module Concur.React.Props where

import Prelude

import Data.Array (concatMap, intercalate)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React (ReactRef)
import React.DOM.Props as P
import React.SyntheticEvent (SyntheticAnimationEvent, SyntheticClipboardEvent, SyntheticCompositionEvent, SyntheticEvent, SyntheticFocusEvent, SyntheticInputEvent, SyntheticKeyboardEvent, SyntheticMouseEvent, SyntheticTouchEvent, SyntheticTransitionEvent, SyntheticUIEvent, SyntheticWheelEvent)
import Unsafe.Coerce (unsafeCoerce)

data Props a = PrimProp P.Props | Handler ((a -> Effect Unit) -> P.Props)

instance functorProps :: Functor Props where
  map _ (PrimProp p) = PrimProp p
  map f (Handler h) = Handler \k -> h (k <<< f)

-- | Internal. Do not use. Use unsafeMkProp, or unsafeMkPropHandler instead.
mkProp :: forall a. (a -> Effect Unit) -> Props a -> P.Props
mkProp _ (PrimProp a) = a
mkProp h (Handler f) = f h

-- | Construct a custom prop handler
unsafeMkPropHandler :: forall a. String -> Props a
unsafeMkPropHandler s = Handler \f -> P.unsafeMkProps s (mkEffectFn1 f)

-- | Construct a custom key value prop
unsafeMkProp :: forall a. String -> a -> Props a
unsafeMkProp s v = PrimProp (P.unsafeMkProps s v)

-- | Shortcut for the common case of a list of classes
classList :: forall a. Array (Maybe String) -> Props a
classList = className <<< intercalate " " <<< concatMap (maybe [] (\s -> [s]))

-- | Use this to filter the output of an event handler prop.
-- | For example, to only handle the enter key - `filterProp isEnterEvent onKeyDown`
filterProp :: forall a. (a -> Boolean) -> Props a -> Props a
filterProp _ p@(PrimProp _) = p
filterProp ok (Handler g) = Handler \h -> (g \a ->
  if ok a then h a else pure unit)

-- FFI + Util stuff
-- | Get the event target's current value
-- | HACK: This is brittle thanks to React's event object reuse!
-- | Safest is to use it directly on the prop like `unsafeTargetValue <$> onKeyDown`
unsafeTargetValue :: SyntheticKeyboardEvent -> String
unsafeTargetValue e = (unsafeCoerce e).target.value

-- | Check if a keyboard event was Enter
isEnterEvent :: SyntheticKeyboardEvent -> Boolean
isEnterEvent e =
  e'.which == 13 || e'.keyCode == 13
  where e' = unsafeCoerce e

-- | IMPORTANT: UNSAFE: It's unsafe to use this outside this module
foreign import resetTargetValue :: forall event. String -> event -> Effect Unit

-- The Standard Set of Props

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

onAnimationStart :: Props SyntheticAnimationEvent
onAnimationStart = Handler P.onAnimationStart

onAnimationEnd :: Props SyntheticAnimationEvent
onAnimationEnd = Handler P.onAnimationEnd

onAnimationIteration :: Props SyntheticAnimationEvent
onAnimationIteration = Handler P.onAnimationIteration

onTransitionEnd :: Props SyntheticTransitionEvent
onTransitionEnd = Handler P.onTransitionEnd

onToggle :: Props SyntheticEvent
onToggle = Handler P.onToggle

onError :: Props SyntheticEvent
onError = Handler P.onError

onLoad :: Props SyntheticEvent
onLoad = Handler P.onLoad

onAbort :: Props SyntheticEvent
onAbort = Handler P.onAbort

onCanPlay :: Props SyntheticEvent
onCanPlay = Handler P.onCanPlay

onCanPlayThrough :: Props SyntheticEvent
onCanPlayThrough = Handler P.onCanPlayThrough

onDurationChange :: Props SyntheticEvent
onDurationChange = Handler P.onDurationChange

onEmptied :: Props SyntheticEvent
onEmptied = Handler P.onEmptied

onEncrypted :: Props SyntheticEvent
onEncrypted = Handler P.onEncrypted

onEnded :: Props SyntheticEvent
onEnded = Handler P.onEnded

onLoadedData :: Props SyntheticEvent
onLoadedData = Handler P.onLoadedData

onLoadedMetadata :: Props SyntheticEvent
onLoadedMetadata = Handler P.onLoadedMetadata

onLoadStart :: Props SyntheticEvent
onLoadStart = Handler P.onLoadStart

onPause :: Props SyntheticEvent
onPause = Handler P.onPause

onPlay :: Props SyntheticEvent
onPlay = Handler P.onPlay

onPlaying :: Props SyntheticEvent
onPlaying = Handler P.onPlaying

onProgress :: Props SyntheticEvent
onProgress = Handler P.onProgress

onRateChange :: Props SyntheticEvent
onRateChange = Handler P.onRateChange

onSeeked :: Props SyntheticEvent
onSeeked = Handler P.onSeeked

onSeeking :: Props SyntheticEvent
onSeeking = Handler P.onSeeking

onStalled :: Props SyntheticEvent
onStalled = Handler P.onStalled

onSuspend :: Props SyntheticEvent
onSuspend = Handler P.onSuspend

onTimeUpdate :: Props SyntheticEvent
onTimeUpdate = Handler P.onTimeUpdate

onVolumeChange :: Props SyntheticEvent
onVolumeChange = Handler P.onVolumeChange

onWaiting :: Props SyntheticEvent
onWaiting = Handler P.onWaiting

onCopy :: Props SyntheticClipboardEvent
onCopy = Handler P.onCopy

onCut :: Props SyntheticClipboardEvent
onCut = Handler P.onCut

onPaste :: Props SyntheticClipboardEvent
onPaste = Handler P.onPaste

onCompositionEnd :: Props SyntheticCompositionEvent
onCompositionEnd = Handler P.onCompositionEnd

onCompositionStart :: Props SyntheticCompositionEvent
onCompositionStart = Handler P.onCompositionStart

onCompositionUpdate :: Props SyntheticCompositionEvent
onCompositionUpdate = Handler P.onCompositionUpdate

onKeyDown :: Props SyntheticKeyboardEvent
onKeyDown = Handler P.onKeyDown

onKeyPress :: Props SyntheticKeyboardEvent
onKeyPress = Handler P.onKeyPress

onKeyUp :: Props SyntheticKeyboardEvent
onKeyUp = Handler P.onKeyUp

onKeyEnter :: Props SyntheticKeyboardEvent
onKeyEnter = filterProp isEnterEvent onKeyDown

onFocus :: Props SyntheticFocusEvent
onFocus = Handler P.onFocus

onBlur :: Props SyntheticFocusEvent
onBlur = Handler P.onBlur

onChange :: Props SyntheticInputEvent
onChange = Handler P.onChange

onInput :: Props SyntheticInputEvent
onInput = Handler P.onInput

onInvalid :: Props SyntheticInputEvent
onInvalid = Handler P.onInvalid

onSubmit :: Props SyntheticInputEvent
onSubmit = Handler P.onSubmit

onClick :: Props SyntheticMouseEvent
onClick = Handler P.onClick

onContextMenu :: Props SyntheticMouseEvent
onContextMenu = Handler P.onContextMenu

onDoubleClick :: Props SyntheticMouseEvent
onDoubleClick = Handler P.onDoubleClick

onDrag :: Props SyntheticMouseEvent
onDrag = Handler P.onDrag

onDragEnd :: Props SyntheticMouseEvent
onDragEnd = Handler P.onDragEnd

onDragEnter :: Props SyntheticMouseEvent
onDragEnter = Handler P.onDragEnter

onDragExit :: Props SyntheticMouseEvent
onDragExit = Handler P.onDragExit

onDragLeave :: Props SyntheticMouseEvent
onDragLeave = Handler P.onDragLeave

onDragOver :: Props SyntheticMouseEvent
onDragOver = Handler P.onDragOver

onDragStart :: Props SyntheticMouseEvent
onDragStart = Handler P.onDragStart

onDrop :: Props SyntheticMouseEvent
onDrop = Handler P.onDrop

onMouseDown :: Props SyntheticMouseEvent
onMouseDown = Handler P.onMouseDown

onMouseEnter :: Props SyntheticMouseEvent
onMouseEnter = Handler P.onMouseEnter

onMouseLeave :: Props SyntheticMouseEvent
onMouseLeave = Handler P.onMouseLeave

onMouseMove :: Props SyntheticMouseEvent
onMouseMove = Handler P.onMouseMove

onMouseOut :: Props SyntheticMouseEvent
onMouseOut = Handler P.onMouseOut

onMouseOver :: Props SyntheticMouseEvent
onMouseOver = Handler P.onMouseOver

onMouseUp :: Props SyntheticMouseEvent
onMouseUp = Handler P.onMouseUp

onSelect :: Props SyntheticEvent
onSelect = Handler P.onSelect

onTouchCancel :: Props SyntheticTouchEvent
onTouchCancel = Handler P.onTouchCancel

onTouchEnd :: Props SyntheticTouchEvent
onTouchEnd = Handler P.onTouchEnd

onTouchMove :: Props SyntheticTouchEvent
onTouchMove = Handler P.onTouchMove

onTouchStart :: Props SyntheticTouchEvent
onTouchStart = Handler P.onTouchStart

onScroll :: Props SyntheticUIEvent
onScroll = Handler P.onScroll

onWheel :: Props SyntheticWheelEvent
onWheel = Handler P.onWheel

onAnimationStartCapture :: Props SyntheticAnimationEvent
onAnimationStartCapture = Handler P.onAnimationStartCapture

onAnimationEndCapture :: Props SyntheticAnimationEvent
onAnimationEndCapture = Handler P.onAnimationEndCapture

onAnimationIterationCapture :: Props SyntheticAnimationEvent
onAnimationIterationCapture = Handler P.onAnimationIterationCapture

onTransitionEndCapture :: Props SyntheticTransitionEvent
onTransitionEndCapture = Handler P.onTransitionEndCapture

onToggleCapture :: Props SyntheticEvent
onToggleCapture = Handler P.onToggleCapture

onErrorCapture :: Props SyntheticEvent
onErrorCapture = Handler P.onErrorCapture

onLoadCapture :: Props SyntheticEvent
onLoadCapture = Handler P.onLoadCapture

onAbortCapture :: Props SyntheticEvent
onAbortCapture = Handler P.onAbortCapture

onCanPlayCapture :: Props SyntheticEvent
onCanPlayCapture = Handler P.onCanPlayCapture

onCanPlayThroughCapture :: Props SyntheticEvent
onCanPlayThroughCapture = Handler P.onCanPlayThroughCapture

onDurationChangeCapture :: Props SyntheticEvent
onDurationChangeCapture = Handler P.onDurationChangeCapture

onEmptiedCapture :: Props SyntheticEvent
onEmptiedCapture = Handler P.onEmptiedCapture

onEncryptedCapture :: Props SyntheticEvent
onEncryptedCapture = Handler P.onEncryptedCapture

onEndedCapture :: Props SyntheticEvent
onEndedCapture = Handler P.onEndedCapture

onLoadedDataCapture :: Props SyntheticEvent
onLoadedDataCapture = Handler P.onLoadedDataCapture

onLoadedMetadataCapture :: Props SyntheticEvent
onLoadedMetadataCapture = Handler P.onLoadedMetadataCapture

onLoadStartCapture :: Props SyntheticEvent
onLoadStartCapture = Handler P.onLoadStartCapture

onPauseCapture :: Props SyntheticEvent
onPauseCapture = Handler P.onPauseCapture

onPlayCapture :: Props SyntheticEvent
onPlayCapture = Handler P.onPlayCapture

onPlayingCapture :: Props SyntheticEvent
onPlayingCapture = Handler P.onPlayingCapture

onProgressCapture :: Props SyntheticEvent
onProgressCapture = Handler P.onProgressCapture

onRateChangeCapture :: Props SyntheticEvent
onRateChangeCapture = Handler P.onRateChangeCapture

onSeekedCapture :: Props SyntheticEvent
onSeekedCapture = Handler P.onSeekedCapture

onSeekingCapture :: Props SyntheticEvent
onSeekingCapture = Handler P.onSeekingCapture

onStalledCapture :: Props SyntheticEvent
onStalledCapture = Handler P.onStalledCapture

onSuspendCapture :: Props SyntheticEvent
onSuspendCapture = Handler P.onSuspendCapture

onTimeUpdateCapture :: Props SyntheticEvent
onTimeUpdateCapture = Handler P.onTimeUpdateCapture

onVolumeChangeCapture :: Props SyntheticEvent
onVolumeChangeCapture = Handler P.onVolumeChangeCapture

onWaitingCapture :: Props SyntheticEvent
onWaitingCapture = Handler P.onWaitingCapture

onCopyCapture :: Props SyntheticClipboardEvent
onCopyCapture = Handler P.onCopyCapture

onCutCapture :: Props SyntheticClipboardEvent
onCutCapture = Handler P.onCutCapture

onPasteCapture :: Props SyntheticClipboardEvent
onPasteCapture = Handler P.onPasteCapture

onCompositionEndCapture :: Props SyntheticCompositionEvent
onCompositionEndCapture = Handler P.onCompositionEndCapture

onCompositionStartCapture :: Props SyntheticCompositionEvent
onCompositionStartCapture = Handler P.onCompositionStartCapture

onCompositionUpdateCapture :: Props SyntheticCompositionEvent
onCompositionUpdateCapture = Handler P.onCompositionUpdateCapture

onKeyDownCapture :: Props SyntheticKeyboardEvent
onKeyDownCapture = Handler P.onKeyDownCapture

onKeyPressCapture :: Props SyntheticKeyboardEvent
onKeyPressCapture = Handler P.onKeyPressCapture

onKeyUpCapture :: Props SyntheticKeyboardEvent
onKeyUpCapture = Handler P.onKeyUpCapture

onFocusCapture :: Props SyntheticFocusEvent
onFocusCapture = Handler P.onFocusCapture

onBlurCapture :: Props SyntheticFocusEvent
onBlurCapture = Handler P.onBlurCapture

onChangeCapture :: Props SyntheticInputEvent
onChangeCapture = Handler P.onChangeCapture

onInputCapture :: Props SyntheticInputEvent
onInputCapture = Handler P.onInputCapture

onInvalidCapture :: Props SyntheticInputEvent
onInvalidCapture = Handler P.onInvalidCapture

onSubmitCapture :: Props SyntheticInputEvent
onSubmitCapture = Handler P.onSubmitCapture

onClickCapture :: Props SyntheticMouseEvent
onClickCapture = Handler P.onClickCapture

onContextMenuCapture :: Props SyntheticMouseEvent
onContextMenuCapture = Handler P.onContextMenuCapture

onDoubleClickCapture :: Props SyntheticMouseEvent
onDoubleClickCapture = Handler P.onDoubleClickCapture

onDragCapture :: Props SyntheticMouseEvent
onDragCapture = Handler P.onDragCapture

onDragEndCapture :: Props SyntheticMouseEvent
onDragEndCapture = Handler P.onDragEndCapture

onDragEnterCapture :: Props SyntheticMouseEvent
onDragEnterCapture = Handler P.onDragEnterCapture

onDragExitCapture :: Props SyntheticMouseEvent
onDragExitCapture = Handler P.onDragExitCapture

onDragLeaveCapture :: Props SyntheticMouseEvent
onDragLeaveCapture = Handler P.onDragLeaveCapture

onDragOverCapture :: Props SyntheticMouseEvent
onDragOverCapture = Handler P.onDragOverCapture

onDragStartCapture :: Props SyntheticMouseEvent
onDragStartCapture = Handler P.onDragStartCapture

onDropCapture :: Props SyntheticMouseEvent
onDropCapture = Handler P.onDropCapture

onMouseDownCapture :: Props SyntheticMouseEvent
onMouseDownCapture = Handler P.onMouseDownCapture

onMouseEnterCapture :: Props SyntheticMouseEvent
onMouseEnterCapture = Handler P.onMouseEnterCapture

onMouseLeaveCapture :: Props SyntheticMouseEvent
onMouseLeaveCapture = Handler P.onMouseLeaveCapture

onMouseMoveCapture :: Props SyntheticMouseEvent
onMouseMoveCapture = Handler P.onMouseMoveCapture

onMouseOutCapture :: Props SyntheticMouseEvent
onMouseOutCapture = Handler P.onMouseOutCapture

onMouseOverCapture :: Props SyntheticMouseEvent
onMouseOverCapture = Handler P.onMouseOverCapture

onMouseUpCapture :: Props SyntheticMouseEvent
onMouseUpCapture = Handler P.onMouseUpCapture

onSelectCapture :: Props SyntheticEvent
onSelectCapture = Handler P.onSelectCapture

onTouchCancelCapture :: Props SyntheticTouchEvent
onTouchCancelCapture = Handler P.onTouchCancelCapture

onTouchEndCapture :: Props SyntheticTouchEvent
onTouchEndCapture = Handler P.onTouchEndCapture

onTouchMoveCapture :: Props SyntheticTouchEvent
onTouchMoveCapture = Handler P.onTouchMoveCapture

onTouchStartCapture :: Props SyntheticTouchEvent
onTouchStartCapture = Handler P.onTouchStartCapture

onScrollCapture :: Props SyntheticUIEvent
onScrollCapture = Handler P.onScrollCapture

onWheelCapture :: Props SyntheticWheelEvent
onWheelCapture = Handler P.onWheelCapture

ref :: Props (Nullable ReactRef)
ref = Handler P.ref

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
