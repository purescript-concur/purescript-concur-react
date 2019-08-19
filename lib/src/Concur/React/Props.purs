module Concur.React.Props where

import Prelude

import Concur.Core.Props (Props(..), filterProp)
import Data.Array (concatMap, intercalate)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import React.Ref as Ref
import React.DOM.Props as P
import React.SyntheticEvent
  ( SyntheticAnimationEvent
  , SyntheticClipboardEvent
  , SyntheticCompositionEvent
  , SyntheticEvent
  , SyntheticEvent_
  , SyntheticFocusEvent
  , SyntheticInputEvent
  , SyntheticKeyboardEvent
  , SyntheticMouseEvent
  , SyntheticTouchEvent
  , SyntheticTransitionEvent
  , SyntheticUIEvent
  , SyntheticWheelEvent
  )
import Unsafe.Coerce (unsafeCoerce)

foreign import emptyProp_ :: P.Props

type ReactProps a = Props P.Props a

emptyProp :: forall a. ReactProps a
emptyProp = PrimProp emptyProp_

-- | Construct a custom prop handler
unsafeMkPropHandler ::
  forall a.
  String ->
  ReactProps a
unsafeMkPropHandler s = Handler \f ->
  P.unsafeMkProps s (mkEffectFn1 f)

-- | Construct a custom key value prop
unsafeMkProp ::
  forall a b.
  String ->
  a ->
  ReactProps b
unsafeMkProp s v = PrimProp (P.unsafeMkProps s v)

-- | Shortcut for the common case of a list of classes
classList ::
  forall a.
  Array (Maybe String) ->
  ReactProps a
classList = className <<< intercalate " " <<< concatMap (maybe [] (\s ->
  [s]))

-- FFI + Util stuff
-- | Get the event target's current value
-- | HACK: This is brittle thanks to React's event object reuse!
-- | Safest is to use it directly on the prop like `unsafeTargetValue <$> onKeyDown`
unsafeTargetValue ::
  forall r.
  SyntheticEvent_ r ->
  String
unsafeTargetValue e = (unsafeCoerce e).target.value

-- | Check if a keyboard event was Enter
isEnterEvent ::
  SyntheticKeyboardEvent ->
  Boolean
isEnterEvent e = e'.which == 13 || e'.keyCode == 13
  where
  e' = unsafeCoerce e

-- | IMPORTANT: UNSAFE: It's unsafe to use this outside this module
foreign import resetTargetValue :: forall event. String -> event -> Effect Unit

-- The Standard Set of Props
aria ::
  forall ariaAttrs a.
  { | ariaAttrs} ->
  ReactProps a
aria = PrimProp <<< P.aria

_data :: forall dataAttrs a. { | dataAttrs} -> ReactProps a
_data = PrimProp <<< P._data

style :: forall style a. { | style} -> ReactProps a
style = PrimProp <<< P.style

dangerouslySetInnerHTML :: forall a. {__html :: String} -> ReactProps a
dangerouslySetInnerHTML = PrimProp <<< P.dangerouslySetInnerHTML

accept :: forall a. String -> ReactProps a
accept = PrimProp <<< P.accept

acceptCharset :: forall a. String -> ReactProps a
acceptCharset = PrimProp <<< P.acceptCharset

accessKey :: forall a. String -> ReactProps a
accessKey = PrimProp <<< P.accessKey

action :: forall a. String -> ReactProps a
action = PrimProp <<< P.action

allowFullScreen :: forall a. Boolean -> ReactProps a
allowFullScreen = PrimProp <<< P.allowFullScreen

allowTransparency :: forall a. Boolean -> ReactProps a
allowTransparency = PrimProp <<< P.allowTransparency

alt :: forall a. String -> ReactProps a
alt = PrimProp <<< P.alt

async :: forall a. Boolean -> ReactProps a
async = PrimProp <<< P.async

autoComplete :: forall a. String -> ReactProps a
autoComplete = PrimProp <<< P.autoComplete

autoFocus :: forall a. Boolean -> ReactProps a
autoFocus = PrimProp <<< P.autoFocus

autoPlay :: forall a. Boolean -> ReactProps a
autoPlay = PrimProp <<< P.autoPlay

capture :: forall a. Boolean -> ReactProps a
capture = PrimProp <<< P.capture

cellPadding :: forall a. String -> ReactProps a
cellPadding = PrimProp <<< P.cellPadding

cellSpacing :: forall a. String -> ReactProps a
cellSpacing = PrimProp <<< P.cellSpacing

charSet :: forall a. String -> ReactProps a
charSet = PrimProp <<< P.charSet

challenge :: forall a. String -> ReactProps a
challenge = PrimProp <<< P.challenge

checked :: forall a. Boolean -> ReactProps a
checked = PrimProp <<< P.checked

cite :: forall a. String -> ReactProps a
cite = PrimProp <<< P.cite

classID :: forall a. String -> ReactProps a
classID = PrimProp <<< P.classID

className :: forall a. String -> ReactProps a
className = PrimProp <<< P.className

cols :: forall a. Int -> ReactProps a
cols = PrimProp <<< P.cols

colSpan :: forall a. Int -> ReactProps a
colSpan = PrimProp <<< P.colSpan

content :: forall a. String -> ReactProps a
content = PrimProp <<< P.content

contentEditable :: forall a. Boolean -> ReactProps a
contentEditable = PrimProp <<< P.contentEditable

contextMenu :: forall a. String -> ReactProps a
contextMenu = PrimProp <<< P.contextMenu

controls :: forall a. Boolean -> ReactProps a
controls = PrimProp <<< P.controls

coords :: forall a. String -> ReactProps a
coords = PrimProp <<< P.coords

crossOrigin :: forall a. String -> ReactProps a
crossOrigin = PrimProp <<< P.crossOrigin

dateTime :: forall a. String -> ReactProps a
dateTime = PrimProp <<< P.dateTime

default :: forall a. Boolean -> ReactProps a
default = PrimProp <<< P.default

defaultChecked :: forall a. Boolean -> ReactProps a
defaultChecked = PrimProp <<< P.defaultChecked

defaultValue :: forall a. String -> ReactProps a
defaultValue = PrimProp <<< P.defaultValue

defer :: forall a. Boolean -> ReactProps a
defer = PrimProp <<< P.defer

dir :: forall a. String -> ReactProps a
dir = PrimProp <<< P.dir

disabled :: forall a. Boolean -> ReactProps a
disabled = PrimProp <<< P.disabled

download :: forall a. String -> ReactProps a
download = PrimProp <<< P.download

draggable :: forall a. Boolean -> ReactProps a
draggable = PrimProp <<< P.draggable

encType :: forall a. String -> ReactProps a
encType = PrimProp <<< P.encType

form :: forall a. String -> ReactProps a
form = PrimProp <<< P.form

formAction :: forall a. String -> ReactProps a
formAction = PrimProp <<< P.formAction

formEncType :: forall a. String -> ReactProps a
formEncType = PrimProp <<< P.formEncType

formMethod :: forall a. String -> ReactProps a
formMethod = PrimProp <<< P.formMethod

formNoValidate :: forall a. Boolean -> ReactProps a
formNoValidate = PrimProp <<< P.formNoValidate

formTarget :: forall a. String -> ReactProps a
formTarget = PrimProp <<< P.formTarget

frameBorder :: forall a. String -> ReactProps a
frameBorder = PrimProp <<< P.frameBorder

headers :: forall a. String -> ReactProps a
headers = PrimProp <<< P.headers

height :: forall a. String -> ReactProps a
height = PrimProp <<< P.height

hidden :: forall a. Boolean -> ReactProps a
hidden = PrimProp <<< P.hidden

high :: forall a. String -> ReactProps a
high = PrimProp <<< P.high

href :: forall a. String -> ReactProps a
href = PrimProp <<< P.href

hrefLang :: forall a. String -> ReactProps a
hrefLang = PrimProp <<< P.hrefLang

htmlFor :: forall a. String -> ReactProps a
htmlFor = PrimProp <<< P.htmlFor

httpEquiv :: forall a. String -> ReactProps a
httpEquiv = PrimProp <<< P.httpEquiv

icon :: forall a. String -> ReactProps a
icon = PrimProp <<< P.icon

_id :: forall a. String -> ReactProps a
_id = PrimProp <<< P._id

inputMode :: forall a. String -> ReactProps a
inputMode = PrimProp <<< P.inputMode

integrity :: forall a. String -> ReactProps a
integrity = PrimProp <<< P.integrity

is :: forall a. String -> ReactProps a
is = PrimProp <<< P.is

key :: forall a. String -> ReactProps a
key = PrimProp <<< P.key

keyParams :: forall a. String -> ReactProps a
keyParams = PrimProp <<< P.keyParams

keyType :: forall a. String -> ReactProps a
keyType = PrimProp <<< P.keyType

kind :: forall a. String -> ReactProps a
kind = PrimProp <<< P.kind

label :: forall a. String -> ReactProps a
label = PrimProp <<< P.label

lang :: forall a. String -> ReactProps a
lang = PrimProp <<< P.lang

list :: forall a. String -> ReactProps a
list = PrimProp <<< P.list

loop :: forall a. Boolean -> ReactProps a
loop = PrimProp <<< P.loop

low :: forall a. String -> ReactProps a
low = PrimProp <<< P.low

manifest :: forall a. String -> ReactProps a
manifest = PrimProp <<< P.manifest

marginHeight :: forall a. String -> ReactProps a
marginHeight = PrimProp <<< P.marginHeight

marginWidth :: forall a. String -> ReactProps a
marginWidth = PrimProp <<< P.marginWidth

max :: forall a. String -> ReactProps a
max = PrimProp <<< P.max

maxLength :: forall a. String -> ReactProps a
maxLength = PrimProp <<< P.maxLength

media :: forall a. String -> ReactProps a
media = PrimProp <<< P.media

mediaGroup :: forall a. String -> ReactProps a
mediaGroup = PrimProp <<< P.mediaGroup

method :: forall a. String -> ReactProps a
method = PrimProp <<< P.method

min :: forall a. String -> ReactProps a
min = PrimProp <<< P.min

minLength :: forall a. String -> ReactProps a
minLength = PrimProp <<< P.minLength

multiple :: forall a. Boolean -> ReactProps a
multiple = PrimProp <<< P.multiple

muted :: forall a. Boolean -> ReactProps a
muted = PrimProp <<< P.muted

name :: forall a. String -> ReactProps a
name = PrimProp <<< P.name

nonce :: forall a. String -> ReactProps a
nonce = PrimProp <<< P.nonce

noValidate :: forall a. Boolean -> ReactProps a
noValidate = PrimProp <<< P.noValidate

open :: forall a. Boolean -> ReactProps a
open = PrimProp <<< P.open

optimum :: forall a. String -> ReactProps a
optimum = PrimProp <<< P.optimum

pattern :: forall a. String -> ReactProps a
pattern = PrimProp <<< P.pattern

placeholder :: forall a. String -> ReactProps a
placeholder = PrimProp <<< P.placeholder

poster :: forall a. String -> ReactProps a
poster = PrimProp <<< P.poster

preload :: forall a. String -> ReactProps a
preload = PrimProp <<< P.preload

profile :: forall a. String -> ReactProps a
profile = PrimProp <<< P.profile

radioGroup :: forall a. String -> ReactProps a
radioGroup = PrimProp <<< P.radioGroup

readOnly :: forall a. Boolean -> ReactProps a
readOnly = PrimProp <<< P.readOnly

rel :: forall a. String -> ReactProps a
rel = PrimProp <<< P.rel

required :: forall a. Boolean -> ReactProps a
required = PrimProp <<< P.required

reversed :: forall a. Boolean -> ReactProps a
reversed = PrimProp <<< P.reversed

role :: forall a. String -> ReactProps a
role = PrimProp <<< P.role

rows :: forall a. Int -> ReactProps a
rows = PrimProp <<< P.rows

rowSpan :: forall a. Int -> ReactProps a
rowSpan = PrimProp <<< P.rowSpan

sandbox :: forall a. String -> ReactProps a
sandbox = PrimProp <<< P.sandbox

scope :: forall a. String -> ReactProps a
scope = PrimProp <<< P.scope

scoped :: forall a. Boolean -> ReactProps a
scoped = PrimProp <<< P.scoped

scrolling :: forall a. String -> ReactProps a
scrolling = PrimProp <<< P.scrolling

seamless :: forall a. Boolean -> ReactProps a
seamless = PrimProp <<< P.seamless

selected :: forall a. Boolean -> ReactProps a
selected = PrimProp <<< P.selected

shape :: forall a. String -> ReactProps a
shape = PrimProp <<< P.shape

size :: forall a. Int -> ReactProps a
size = PrimProp <<< P.size

sizes :: forall a. String -> ReactProps a
sizes = PrimProp <<< P.sizes

span :: forall a. Int -> ReactProps a
span = PrimProp <<< P.span

spellCheck :: forall a. Boolean -> ReactProps a
spellCheck = PrimProp <<< P.spellCheck

src :: forall a. String -> ReactProps a
src = PrimProp <<< P.src

srcDoc :: forall a. String -> ReactProps a
srcDoc = PrimProp <<< P.srcDoc

srcLang :: forall a. String -> ReactProps a
srcLang = PrimProp <<< P.srcLang

srcSet :: forall a. String -> ReactProps a
srcSet = PrimProp <<< P.srcSet

start :: forall a. Int -> ReactProps a
start = PrimProp <<< P.start

step :: forall a. String -> ReactProps a
step = PrimProp <<< P.step

summary :: forall a. String -> ReactProps a
summary = PrimProp <<< P.summary

tabIndex :: forall a. Int -> ReactProps a
tabIndex = PrimProp <<< P.tabIndex

target :: forall a. String -> ReactProps a
target = PrimProp <<< P.target

title :: forall a. String -> ReactProps a
title = PrimProp <<< P.title

_type :: forall a. String -> ReactProps a
_type = PrimProp <<< P._type

useMap :: forall a. String -> ReactProps a
useMap = PrimProp <<< P.useMap

value :: forall a. String -> ReactProps a
value = PrimProp <<< P.value

valueArray :: forall a. Array String -> ReactProps a
valueArray = PrimProp <<< P.valueArray

width :: forall a. String -> ReactProps a
width = PrimProp <<< P.width

wmode :: forall a. String -> ReactProps a
wmode = PrimProp <<< P.wmode

wrap :: forall a. String -> ReactProps a
wrap = PrimProp <<< P.wrap

-- RDFa Attributes
about ::
  forall a.
  String ->
  ReactProps a
about = PrimProp <<< P.about

datatype :: forall a. String -> ReactProps a
datatype = PrimProp <<< P.datatype

inlist :: forall a. String -> ReactProps a
inlist = PrimProp <<< P.inlist

prefix :: forall a. String -> ReactProps a
prefix = PrimProp <<< P.prefix

property :: forall a. String -> ReactProps a
property = PrimProp <<< P.property

resource :: forall a. String -> ReactProps a
resource = PrimProp <<< P.resource

typeof :: forall a. String -> ReactProps a
typeof = PrimProp <<< P.typeof

vocab :: forall a. String -> ReactProps a
vocab = PrimProp <<< P.vocab

-- Non-standard Attributes
autoCapitalize ::
  forall a.
  String ->
  ReactProps a
autoCapitalize = PrimProp <<< P.autoCapitalize

autoCorrect :: forall a. String -> ReactProps a
autoCorrect = PrimProp <<< P.autoCorrect

autoSave :: forall a. String -> ReactProps a
autoSave = PrimProp <<< P.autoSave

color :: forall a. String -> ReactProps a
color = PrimProp <<< P.color

itemProp :: forall a. String -> ReactProps a
itemProp = PrimProp <<< P.itemProp

itemScope :: forall a. Boolean -> ReactProps a
itemScope = PrimProp <<< P.itemScope

itemType :: forall a. String -> ReactProps a
itemType = PrimProp <<< P.itemType

itemID :: forall a. String -> ReactProps a
itemID = PrimProp <<< P.itemID

itemRef :: forall a. String -> ReactProps a
itemRef = PrimProp <<< P.itemRef

results :: forall a. Int -> ReactProps a
results = PrimProp <<< P.results

security :: forall a. String -> ReactProps a
security = PrimProp <<< P.security

unselectable :: forall a. Boolean -> ReactProps a
unselectable = PrimProp <<< P.unselectable

onAnimationStart :: ReactProps SyntheticAnimationEvent
onAnimationStart = Handler P.onAnimationStart

onAnimationEnd :: ReactProps SyntheticAnimationEvent
onAnimationEnd = Handler P.onAnimationEnd

onAnimationIteration :: ReactProps SyntheticAnimationEvent
onAnimationIteration = Handler P.onAnimationIteration

onTransitionEnd :: ReactProps SyntheticTransitionEvent
onTransitionEnd = Handler P.onTransitionEnd

onToggle :: ReactProps SyntheticEvent
onToggle = Handler P.onToggle

onError :: ReactProps SyntheticEvent
onError = Handler P.onError

onLoad :: ReactProps SyntheticEvent
onLoad = Handler P.onLoad

onAbort :: ReactProps SyntheticEvent
onAbort = Handler P.onAbort

onCanPlay :: ReactProps SyntheticEvent
onCanPlay = Handler P.onCanPlay

onCanPlayThrough :: ReactProps SyntheticEvent
onCanPlayThrough = Handler P.onCanPlayThrough

onDurationChange :: ReactProps SyntheticEvent
onDurationChange = Handler P.onDurationChange

onEmptied :: ReactProps SyntheticEvent
onEmptied = Handler P.onEmptied

onEncrypted :: ReactProps SyntheticEvent
onEncrypted = Handler P.onEncrypted

onEnded :: ReactProps SyntheticEvent
onEnded = Handler P.onEnded

onLoadedData :: ReactProps SyntheticEvent
onLoadedData = Handler P.onLoadedData

onLoadedMetadata :: ReactProps SyntheticEvent
onLoadedMetadata = Handler P.onLoadedMetadata

onLoadStart :: ReactProps SyntheticEvent
onLoadStart = Handler P.onLoadStart

onPause :: ReactProps SyntheticEvent
onPause = Handler P.onPause

onPlay :: ReactProps SyntheticEvent
onPlay = Handler P.onPlay

onPlaying :: ReactProps SyntheticEvent
onPlaying = Handler P.onPlaying

onProgress :: ReactProps SyntheticEvent
onProgress = Handler P.onProgress

onRateChange :: ReactProps SyntheticEvent
onRateChange = Handler P.onRateChange

onSeeked :: ReactProps SyntheticEvent
onSeeked = Handler P.onSeeked

onSeeking :: ReactProps SyntheticEvent
onSeeking = Handler P.onSeeking

onStalled :: ReactProps SyntheticEvent
onStalled = Handler P.onStalled

onSuspend :: ReactProps SyntheticEvent
onSuspend = Handler P.onSuspend

onTimeUpdate :: ReactProps SyntheticEvent
onTimeUpdate = Handler P.onTimeUpdate

onVolumeChange :: ReactProps SyntheticEvent
onVolumeChange = Handler P.onVolumeChange

onWaiting :: ReactProps SyntheticEvent
onWaiting = Handler P.onWaiting

onCopy :: ReactProps SyntheticClipboardEvent
onCopy = Handler P.onCopy

onCut :: ReactProps SyntheticClipboardEvent
onCut = Handler P.onCut

onPaste :: ReactProps SyntheticClipboardEvent
onPaste = Handler P.onPaste

onCompositionEnd :: ReactProps SyntheticCompositionEvent
onCompositionEnd = Handler P.onCompositionEnd

onCompositionStart :: ReactProps SyntheticCompositionEvent
onCompositionStart = Handler P.onCompositionStart

onCompositionUpdate :: ReactProps SyntheticCompositionEvent
onCompositionUpdate = Handler P.onCompositionUpdate

onKeyDown :: ReactProps SyntheticKeyboardEvent
onKeyDown = Handler P.onKeyDown

onKeyPress :: ReactProps SyntheticKeyboardEvent
onKeyPress = Handler P.onKeyPress

onKeyUp :: ReactProps SyntheticKeyboardEvent
onKeyUp = Handler P.onKeyUp

onKeyEnter :: ReactProps SyntheticKeyboardEvent
onKeyEnter = filterProp isEnterEvent onKeyDown

onFocus :: ReactProps SyntheticFocusEvent
onFocus = Handler P.onFocus

onBlur :: ReactProps SyntheticFocusEvent
onBlur = Handler P.onBlur

onChange :: ReactProps SyntheticInputEvent
onChange = Handler P.onChange

onInput :: ReactProps SyntheticInputEvent
onInput = Handler P.onInput

onInvalid :: ReactProps SyntheticInputEvent
onInvalid = Handler P.onInvalid

onSubmit :: ReactProps SyntheticInputEvent
onSubmit = Handler P.onSubmit

onClick :: ReactProps SyntheticMouseEvent
onClick = Handler P.onClick

onContextMenu :: ReactProps SyntheticMouseEvent
onContextMenu = Handler P.onContextMenu

onDoubleClick :: ReactProps SyntheticMouseEvent
onDoubleClick = Handler P.onDoubleClick

onDrag :: ReactProps SyntheticMouseEvent
onDrag = Handler P.onDrag

onDragEnd :: ReactProps SyntheticMouseEvent
onDragEnd = Handler P.onDragEnd

onDragEnter :: ReactProps SyntheticMouseEvent
onDragEnter = Handler P.onDragEnter

onDragExit :: ReactProps SyntheticMouseEvent
onDragExit = Handler P.onDragExit

onDragLeave :: ReactProps SyntheticMouseEvent
onDragLeave = Handler P.onDragLeave

onDragOver :: ReactProps SyntheticMouseEvent
onDragOver = Handler P.onDragOver

onDragStart :: ReactProps SyntheticMouseEvent
onDragStart = Handler P.onDragStart

onDrop :: ReactProps SyntheticMouseEvent
onDrop = Handler P.onDrop

onMouseDown :: ReactProps SyntheticMouseEvent
onMouseDown = Handler P.onMouseDown

onMouseEnter :: ReactProps SyntheticMouseEvent
onMouseEnter = Handler P.onMouseEnter

onMouseLeave :: ReactProps SyntheticMouseEvent
onMouseLeave = Handler P.onMouseLeave

onMouseMove :: ReactProps SyntheticMouseEvent
onMouseMove = Handler P.onMouseMove

onMouseOut :: ReactProps SyntheticMouseEvent
onMouseOut = Handler P.onMouseOut

onMouseOver :: ReactProps SyntheticMouseEvent
onMouseOver = Handler P.onMouseOver

onMouseUp :: ReactProps SyntheticMouseEvent
onMouseUp = Handler P.onMouseUp

onSelect :: ReactProps SyntheticEvent
onSelect = Handler P.onSelect

onTouchCancel :: ReactProps SyntheticTouchEvent
onTouchCancel = Handler P.onTouchCancel

onTouchEnd :: ReactProps SyntheticTouchEvent
onTouchEnd = Handler P.onTouchEnd

onTouchMove :: ReactProps SyntheticTouchEvent
onTouchMove = Handler P.onTouchMove

onTouchStart :: ReactProps SyntheticTouchEvent
onTouchStart = Handler P.onTouchStart

onScroll :: ReactProps SyntheticUIEvent
onScroll = Handler P.onScroll

onWheel :: ReactProps SyntheticWheelEvent
onWheel = Handler P.onWheel

onAnimationStartCapture :: ReactProps SyntheticAnimationEvent
onAnimationStartCapture = Handler P.onAnimationStartCapture

onAnimationEndCapture :: ReactProps SyntheticAnimationEvent
onAnimationEndCapture = Handler P.onAnimationEndCapture

onAnimationIterationCapture :: ReactProps SyntheticAnimationEvent
onAnimationIterationCapture = Handler P.onAnimationIterationCapture

onTransitionEndCapture :: ReactProps SyntheticTransitionEvent
onTransitionEndCapture = Handler P.onTransitionEndCapture

onToggleCapture :: ReactProps SyntheticEvent
onToggleCapture = Handler P.onToggleCapture

onErrorCapture :: ReactProps SyntheticEvent
onErrorCapture = Handler P.onErrorCapture

onLoadCapture :: ReactProps SyntheticEvent
onLoadCapture = Handler P.onLoadCapture

onAbortCapture :: ReactProps SyntheticEvent
onAbortCapture = Handler P.onAbortCapture

onCanPlayCapture :: ReactProps SyntheticEvent
onCanPlayCapture = Handler P.onCanPlayCapture

onCanPlayThroughCapture :: ReactProps SyntheticEvent
onCanPlayThroughCapture = Handler P.onCanPlayThroughCapture

onDurationChangeCapture :: ReactProps SyntheticEvent
onDurationChangeCapture = Handler P.onDurationChangeCapture

onEmptiedCapture :: ReactProps SyntheticEvent
onEmptiedCapture = Handler P.onEmptiedCapture

onEncryptedCapture :: ReactProps SyntheticEvent
onEncryptedCapture = Handler P.onEncryptedCapture

onEndedCapture :: ReactProps SyntheticEvent
onEndedCapture = Handler P.onEndedCapture

onLoadedDataCapture :: ReactProps SyntheticEvent
onLoadedDataCapture = Handler P.onLoadedDataCapture

onLoadedMetadataCapture :: ReactProps SyntheticEvent
onLoadedMetadataCapture = Handler P.onLoadedMetadataCapture

onLoadStartCapture :: ReactProps SyntheticEvent
onLoadStartCapture = Handler P.onLoadStartCapture

onPauseCapture :: ReactProps SyntheticEvent
onPauseCapture = Handler P.onPauseCapture

onPlayCapture :: ReactProps SyntheticEvent
onPlayCapture = Handler P.onPlayCapture

onPlayingCapture :: ReactProps SyntheticEvent
onPlayingCapture = Handler P.onPlayingCapture

onProgressCapture :: ReactProps SyntheticEvent
onProgressCapture = Handler P.onProgressCapture

onRateChangeCapture :: ReactProps SyntheticEvent
onRateChangeCapture = Handler P.onRateChangeCapture

onSeekedCapture :: ReactProps SyntheticEvent
onSeekedCapture = Handler P.onSeekedCapture

onSeekingCapture :: ReactProps SyntheticEvent
onSeekingCapture = Handler P.onSeekingCapture

onStalledCapture :: ReactProps SyntheticEvent
onStalledCapture = Handler P.onStalledCapture

onSuspendCapture :: ReactProps SyntheticEvent
onSuspendCapture = Handler P.onSuspendCapture

onTimeUpdateCapture :: ReactProps SyntheticEvent
onTimeUpdateCapture = Handler P.onTimeUpdateCapture

onVolumeChangeCapture :: ReactProps SyntheticEvent
onVolumeChangeCapture = Handler P.onVolumeChangeCapture

onWaitingCapture :: ReactProps SyntheticEvent
onWaitingCapture = Handler P.onWaitingCapture

onCopyCapture :: ReactProps SyntheticClipboardEvent
onCopyCapture = Handler P.onCopyCapture

onCutCapture :: ReactProps SyntheticClipboardEvent
onCutCapture = Handler P.onCutCapture

onPasteCapture :: ReactProps SyntheticClipboardEvent
onPasteCapture = Handler P.onPasteCapture

onCompositionEndCapture :: ReactProps SyntheticCompositionEvent
onCompositionEndCapture = Handler P.onCompositionEndCapture

onCompositionStartCapture :: ReactProps SyntheticCompositionEvent
onCompositionStartCapture = Handler P.onCompositionStartCapture

onCompositionUpdateCapture :: ReactProps SyntheticCompositionEvent
onCompositionUpdateCapture = Handler P.onCompositionUpdateCapture

onKeyDownCapture :: ReactProps SyntheticKeyboardEvent
onKeyDownCapture = Handler P.onKeyDownCapture

onKeyPressCapture :: ReactProps SyntheticKeyboardEvent
onKeyPressCapture = Handler P.onKeyPressCapture

onKeyUpCapture :: ReactProps SyntheticKeyboardEvent
onKeyUpCapture = Handler P.onKeyUpCapture

onFocusCapture :: ReactProps SyntheticFocusEvent
onFocusCapture = Handler P.onFocusCapture

onBlurCapture :: ReactProps SyntheticFocusEvent
onBlurCapture = Handler P.onBlurCapture

onChangeCapture :: ReactProps SyntheticInputEvent
onChangeCapture = Handler P.onChangeCapture

onInputCapture :: ReactProps SyntheticInputEvent
onInputCapture = Handler P.onInputCapture

onInvalidCapture :: ReactProps SyntheticInputEvent
onInvalidCapture = Handler P.onInvalidCapture

onSubmitCapture :: ReactProps SyntheticInputEvent
onSubmitCapture = Handler P.onSubmitCapture

onClickCapture :: ReactProps SyntheticMouseEvent
onClickCapture = Handler P.onClickCapture

onContextMenuCapture :: ReactProps SyntheticMouseEvent
onContextMenuCapture = Handler P.onContextMenuCapture

onDoubleClickCapture :: ReactProps SyntheticMouseEvent
onDoubleClickCapture = Handler P.onDoubleClickCapture

onDragCapture :: ReactProps SyntheticMouseEvent
onDragCapture = Handler P.onDragCapture

onDragEndCapture :: ReactProps SyntheticMouseEvent
onDragEndCapture = Handler P.onDragEndCapture

onDragEnterCapture :: ReactProps SyntheticMouseEvent
onDragEnterCapture = Handler P.onDragEnterCapture

onDragExitCapture :: ReactProps SyntheticMouseEvent
onDragExitCapture = Handler P.onDragExitCapture

onDragLeaveCapture :: ReactProps SyntheticMouseEvent
onDragLeaveCapture = Handler P.onDragLeaveCapture

onDragOverCapture :: ReactProps SyntheticMouseEvent
onDragOverCapture = Handler P.onDragOverCapture

onDragStartCapture :: ReactProps SyntheticMouseEvent
onDragStartCapture = Handler P.onDragStartCapture

onDropCapture :: ReactProps SyntheticMouseEvent
onDropCapture = Handler P.onDropCapture

onMouseDownCapture :: ReactProps SyntheticMouseEvent
onMouseDownCapture = Handler P.onMouseDownCapture

onMouseEnterCapture :: ReactProps SyntheticMouseEvent
onMouseEnterCapture = Handler P.onMouseEnterCapture

onMouseLeaveCapture :: ReactProps SyntheticMouseEvent
onMouseLeaveCapture = Handler P.onMouseLeaveCapture

onMouseMoveCapture :: ReactProps SyntheticMouseEvent
onMouseMoveCapture = Handler P.onMouseMoveCapture

onMouseOutCapture :: ReactProps SyntheticMouseEvent
onMouseOutCapture = Handler P.onMouseOutCapture

onMouseOverCapture :: ReactProps SyntheticMouseEvent
onMouseOverCapture = Handler P.onMouseOverCapture

onMouseUpCapture :: ReactProps SyntheticMouseEvent
onMouseUpCapture = Handler P.onMouseUpCapture

onSelectCapture :: ReactProps SyntheticEvent
onSelectCapture = Handler P.onSelectCapture

onTouchCancelCapture :: ReactProps SyntheticTouchEvent
onTouchCancelCapture = Handler P.onTouchCancelCapture

onTouchEndCapture :: ReactProps SyntheticTouchEvent
onTouchEndCapture = Handler P.onTouchEndCapture

onTouchMoveCapture :: ReactProps SyntheticTouchEvent
onTouchMoveCapture = Handler P.onTouchMoveCapture

onTouchStartCapture :: ReactProps SyntheticTouchEvent
onTouchStartCapture = Handler P.onTouchStartCapture

onScrollCapture :: ReactProps SyntheticUIEvent
onScrollCapture = Handler P.onScrollCapture

onWheelCapture :: ReactProps SyntheticWheelEvent
onWheelCapture = Handler P.onWheelCapture

ref :: forall a. Ref.RefHandler Ref.NativeNode -> ReactProps a
ref = PrimProp <<< P.ref

suppressContentEditableWarning :: forall a. Boolean -> ReactProps a
suppressContentEditableWarning = PrimProp <<< P.suppressContentEditableWarning

-- SVG attributes
x ::
  forall a.
  Int ->
  ReactProps a
x = PrimProp <<< P.x

y :: forall a. Int -> ReactProps a
y = PrimProp <<< P.y

cx :: forall a. Int -> ReactProps a
cx = PrimProp <<< P.cx

cy :: forall a. Int -> ReactProps a
cy = PrimProp <<< P.cy

r :: forall a. Int -> ReactProps a
r = PrimProp <<< P.r

fill :: forall a. String -> ReactProps a
fill = PrimProp <<< P.fill

opacity :: forall a. Int -> ReactProps a
opacity = PrimProp <<< P.opacity

fillOpacity :: forall a. Int -> ReactProps a
fillOpacity = PrimProp <<< P.fillOpacity

stroke :: forall a. String -> ReactProps a
stroke = PrimProp <<< P.stroke

strokeWidth :: forall a. Int -> ReactProps a
strokeWidth = PrimProp <<< P.strokeWidth

points :: forall a. String -> ReactProps a
points = PrimProp <<< P.points

d :: forall a. String -> ReactProps a
d = PrimProp <<< P.d

viewBox :: forall a. String -> ReactProps a
viewBox = PrimProp <<< P.viewBox
