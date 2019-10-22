module Concur.Stark.Props where

import Prelude

import Concur.Core.Props (Props(..), filterProp)
import Data.Array (concatMap, intercalate)
import Data.Maybe (Maybe, maybe)
import Effect.Uncurried (mkEffectFn1)
import Stark.Ref as Ref
import Stark.DOM.Props as P
import Stark.SyntheticEvent
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

-- emptyProp_ :: P.Props
-- emptyProp_ = P.Props

type StarkProps a = Props P.Props a

-- emptyProp :: forall a. StarkProps a
-- emptyProp = PrimProp emptyProp_

-- | Construct a custom prop handler
unsafeMkPropHandler ::
  forall a.
  String ->
  StarkProps a
unsafeMkPropHandler s = Handler \f ->
  P.unsafeMkProps s (mkEffectFn1 f)

-- | Construct a custom key value prop
unsafeMkProp ::
  forall a b.
  String ->
  a ->
  StarkProps b
unsafeMkProp s v = PrimProp (P.unsafeMkProps s v)

-- | Shortcut for the common case of a list of classes
classList ::
  forall a.
  Array (Maybe String) ->
  StarkProps a
classList = className <<< intercalate " " <<< concatMap (maybe [] (\s ->
  [s]))

-- FFI + Util stuff
-- | Get the event target's current value
-- | HACK: This is brittle thanks to Stark's event object reuse!
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
-- foreign import resetTargetValue :: forall event. String -> event -> Effect Unit

-- The Standard Set of Props
aria ::
  forall ariaAttrs a.
  { | ariaAttrs} ->
  StarkProps a
aria = PrimProp <<< P.aria

_data :: forall dataAttrs a. { | dataAttrs} -> StarkProps a
_data = PrimProp <<< P._data

style :: forall style a. { | style} -> StarkProps a
style = PrimProp <<< P.style

dangerouslySetInnerHTML :: forall a. {__html :: String} -> StarkProps a
dangerouslySetInnerHTML = PrimProp <<< P.dangerouslySetInnerHTML

accept :: forall a. String -> StarkProps a
accept = PrimProp <<< P.accept

acceptCharset :: forall a. String -> StarkProps a
acceptCharset = PrimProp <<< P.acceptCharset

accessKey :: forall a. String -> StarkProps a
accessKey = PrimProp <<< P.accessKey

action :: forall a. String -> StarkProps a
action = PrimProp <<< P.action

allowFullScreen :: forall a. Boolean -> StarkProps a
allowFullScreen = PrimProp <<< P.allowFullScreen

allowTransparency :: forall a. Boolean -> StarkProps a
allowTransparency = PrimProp <<< P.allowTransparency

alt :: forall a. String -> StarkProps a
alt = PrimProp <<< P.alt

async :: forall a. Boolean -> StarkProps a
async = PrimProp <<< P.async

autoComplete :: forall a. String -> StarkProps a
autoComplete = PrimProp <<< P.autoComplete

autoFocus :: forall a. Boolean -> StarkProps a
autoFocus = PrimProp <<< P.autoFocus

autoPlay :: forall a. Boolean -> StarkProps a
autoPlay = PrimProp <<< P.autoPlay

capture :: forall a. Boolean -> StarkProps a
capture = PrimProp <<< P.capture

cellPadding :: forall a. String -> StarkProps a
cellPadding = PrimProp <<< P.cellPadding

cellSpacing :: forall a. String -> StarkProps a
cellSpacing = PrimProp <<< P.cellSpacing

charSet :: forall a. String -> StarkProps a
charSet = PrimProp <<< P.charSet

challenge :: forall a. String -> StarkProps a
challenge = PrimProp <<< P.challenge

checked :: forall a. Boolean -> StarkProps a
checked = PrimProp <<< P.checked

cite :: forall a. String -> StarkProps a
cite = PrimProp <<< P.cite

classID :: forall a. String -> StarkProps a
classID = PrimProp <<< P.classID

className :: forall a. String -> StarkProps a
className = PrimProp <<< P.className

cols :: forall a. Int -> StarkProps a
cols = PrimProp <<< P.cols

colSpan :: forall a. Int -> StarkProps a
colSpan = PrimProp <<< P.colSpan

content :: forall a. String -> StarkProps a
content = PrimProp <<< P.content

contentEditable :: forall a. Boolean -> StarkProps a
contentEditable = PrimProp <<< P.contentEditable

contextMenu :: forall a. String -> StarkProps a
contextMenu = PrimProp <<< P.contextMenu

controls :: forall a. Boolean -> StarkProps a
controls = PrimProp <<< P.controls

coords :: forall a. String -> StarkProps a
coords = PrimProp <<< P.coords

crossOrigin :: forall a. String -> StarkProps a
crossOrigin = PrimProp <<< P.crossOrigin

dateTime :: forall a. String -> StarkProps a
dateTime = PrimProp <<< P.dateTime

default :: forall a. Boolean -> StarkProps a
default = PrimProp <<< P.default

defaultChecked :: forall a. Boolean -> StarkProps a
defaultChecked = PrimProp <<< P.defaultChecked

defaultValue :: forall a. String -> StarkProps a
defaultValue = PrimProp <<< P.defaultValue

defer :: forall a. Boolean -> StarkProps a
defer = PrimProp <<< P.defer

dir :: forall a. String -> StarkProps a
dir = PrimProp <<< P.dir

disabled :: forall a. Boolean -> StarkProps a
disabled = PrimProp <<< P.disabled

download :: forall a. String -> StarkProps a
download = PrimProp <<< P.download

draggable :: forall a. Boolean -> StarkProps a
draggable = PrimProp <<< P.draggable

encType :: forall a. String -> StarkProps a
encType = PrimProp <<< P.encType

form :: forall a. String -> StarkProps a
form = PrimProp <<< P.form

formAction :: forall a. String -> StarkProps a
formAction = PrimProp <<< P.formAction

formEncType :: forall a. String -> StarkProps a
formEncType = PrimProp <<< P.formEncType

formMethod :: forall a. String -> StarkProps a
formMethod = PrimProp <<< P.formMethod

formNoValidate :: forall a. Boolean -> StarkProps a
formNoValidate = PrimProp <<< P.formNoValidate

formTarget :: forall a. String -> StarkProps a
formTarget = PrimProp <<< P.formTarget

frameBorder :: forall a. String -> StarkProps a
frameBorder = PrimProp <<< P.frameBorder

headers :: forall a. String -> StarkProps a
headers = PrimProp <<< P.headers

height :: forall a. String -> StarkProps a
height = PrimProp <<< P.height

hidden :: forall a. Boolean -> StarkProps a
hidden = PrimProp <<< P.hidden

high :: forall a. String -> StarkProps a
high = PrimProp <<< P.high

href :: forall a. String -> StarkProps a
href = PrimProp <<< P.href

hrefLang :: forall a. String -> StarkProps a
hrefLang = PrimProp <<< P.hrefLang

htmlFor :: forall a. String -> StarkProps a
htmlFor = PrimProp <<< P.htmlFor

httpEquiv :: forall a. String -> StarkProps a
httpEquiv = PrimProp <<< P.httpEquiv

icon :: forall a. String -> StarkProps a
icon = PrimProp <<< P.icon

_id :: forall a. String -> StarkProps a
_id = PrimProp <<< P._id

inputMode :: forall a. String -> StarkProps a
inputMode = PrimProp <<< P.inputMode

integrity :: forall a. String -> StarkProps a
integrity = PrimProp <<< P.integrity

is :: forall a. String -> StarkProps a
is = PrimProp <<< P.is

key :: forall a. String -> StarkProps a
key = PrimProp <<< P.key

keyParams :: forall a. String -> StarkProps a
keyParams = PrimProp <<< P.keyParams

keyType :: forall a. String -> StarkProps a
keyType = PrimProp <<< P.keyType

kind :: forall a. String -> StarkProps a
kind = PrimProp <<< P.kind

label :: forall a. String -> StarkProps a
label = PrimProp <<< P.label

lang :: forall a. String -> StarkProps a
lang = PrimProp <<< P.lang

list :: forall a. String -> StarkProps a
list = PrimProp <<< P.list

loop :: forall a. Boolean -> StarkProps a
loop = PrimProp <<< P.loop

low :: forall a. String -> StarkProps a
low = PrimProp <<< P.low

manifest :: forall a. String -> StarkProps a
manifest = PrimProp <<< P.manifest

marginHeight :: forall a. String -> StarkProps a
marginHeight = PrimProp <<< P.marginHeight

marginWidth :: forall a. String -> StarkProps a
marginWidth = PrimProp <<< P.marginWidth

max :: forall a. String -> StarkProps a
max = PrimProp <<< P.max

maxLength :: forall a. String -> StarkProps a
maxLength = PrimProp <<< P.maxLength

media :: forall a. String -> StarkProps a
media = PrimProp <<< P.media

mediaGroup :: forall a. String -> StarkProps a
mediaGroup = PrimProp <<< P.mediaGroup

method :: forall a. String -> StarkProps a
method = PrimProp <<< P.method

min :: forall a. String -> StarkProps a
min = PrimProp <<< P.min

minLength :: forall a. String -> StarkProps a
minLength = PrimProp <<< P.minLength

multiple :: forall a. Boolean -> StarkProps a
multiple = PrimProp <<< P.multiple

muted :: forall a. Boolean -> StarkProps a
muted = PrimProp <<< P.muted

name :: forall a. String -> StarkProps a
name = PrimProp <<< P.name

nonce :: forall a. String -> StarkProps a
nonce = PrimProp <<< P.nonce

noValidate :: forall a. Boolean -> StarkProps a
noValidate = PrimProp <<< P.noValidate

open :: forall a. Boolean -> StarkProps a
open = PrimProp <<< P.open

optimum :: forall a. String -> StarkProps a
optimum = PrimProp <<< P.optimum

pattern :: forall a. String -> StarkProps a
pattern = PrimProp <<< P.pattern

placeholder :: forall a. String -> StarkProps a
placeholder = PrimProp <<< P.placeholder

poster :: forall a. String -> StarkProps a
poster = PrimProp <<< P.poster

preload :: forall a. String -> StarkProps a
preload = PrimProp <<< P.preload

profile :: forall a. String -> StarkProps a
profile = PrimProp <<< P.profile

radioGroup :: forall a. String -> StarkProps a
radioGroup = PrimProp <<< P.radioGroup

readOnly :: forall a. Boolean -> StarkProps a
readOnly = PrimProp <<< P.readOnly

rel :: forall a. String -> StarkProps a
rel = PrimProp <<< P.rel

required :: forall a. Boolean -> StarkProps a
required = PrimProp <<< P.required

reversed :: forall a. Boolean -> StarkProps a
reversed = PrimProp <<< P.reversed

role :: forall a. String -> StarkProps a
role = PrimProp <<< P.role

rows :: forall a. Int -> StarkProps a
rows = PrimProp <<< P.rows

rowSpan :: forall a. Int -> StarkProps a
rowSpan = PrimProp <<< P.rowSpan

sandbox :: forall a. String -> StarkProps a
sandbox = PrimProp <<< P.sandbox

scope :: forall a. String -> StarkProps a
scope = PrimProp <<< P.scope

scoped :: forall a. Boolean -> StarkProps a
scoped = PrimProp <<< P.scoped

scrolling :: forall a. String -> StarkProps a
scrolling = PrimProp <<< P.scrolling

seamless :: forall a. Boolean -> StarkProps a
seamless = PrimProp <<< P.seamless

selected :: forall a. Boolean -> StarkProps a
selected = PrimProp <<< P.selected

shape :: forall a. String -> StarkProps a
shape = PrimProp <<< P.shape

size :: forall a. Int -> StarkProps a
size = PrimProp <<< P.size

sizes :: forall a. String -> StarkProps a
sizes = PrimProp <<< P.sizes

span :: forall a. Int -> StarkProps a
span = PrimProp <<< P.span

spellCheck :: forall a. Boolean -> StarkProps a
spellCheck = PrimProp <<< P.spellCheck

src :: forall a. String -> StarkProps a
src = PrimProp <<< P.src

srcDoc :: forall a. String -> StarkProps a
srcDoc = PrimProp <<< P.srcDoc

srcLang :: forall a. String -> StarkProps a
srcLang = PrimProp <<< P.srcLang

srcSet :: forall a. String -> StarkProps a
srcSet = PrimProp <<< P.srcSet

start :: forall a. Int -> StarkProps a
start = PrimProp <<< P.start

step :: forall a. String -> StarkProps a
step = PrimProp <<< P.step

summary :: forall a. String -> StarkProps a
summary = PrimProp <<< P.summary

tabIndex :: forall a. Int -> StarkProps a
tabIndex = PrimProp <<< P.tabIndex

target :: forall a. String -> StarkProps a
target = PrimProp <<< P.target

title :: forall a. String -> StarkProps a
title = PrimProp <<< P.title

_type :: forall a. String -> StarkProps a
_type = PrimProp <<< P._type

useMap :: forall a. String -> StarkProps a
useMap = PrimProp <<< P.useMap

value :: forall a. String -> StarkProps a
value = PrimProp <<< P.value

valueArray :: forall a. Array String -> StarkProps a
valueArray = PrimProp <<< P.valueArray

width :: forall a. String -> StarkProps a
width = PrimProp <<< P.width

wmode :: forall a. String -> StarkProps a
wmode = PrimProp <<< P.wmode

wrap :: forall a. String -> StarkProps a
wrap = PrimProp <<< P.wrap

-- RDFa Attributes
about ::
  forall a.
  String ->
  StarkProps a
about = PrimProp <<< P.about

datatype :: forall a. String -> StarkProps a
datatype = PrimProp <<< P.datatype

inlist :: forall a. String -> StarkProps a
inlist = PrimProp <<< P.inlist

prefix :: forall a. String -> StarkProps a
prefix = PrimProp <<< P.prefix

property :: forall a. String -> StarkProps a
property = PrimProp <<< P.property

resource :: forall a. String -> StarkProps a
resource = PrimProp <<< P.resource

typeof :: forall a. String -> StarkProps a
typeof = PrimProp <<< P.typeof

vocab :: forall a. String -> StarkProps a
vocab = PrimProp <<< P.vocab

-- Non-standard Attributes
autoCapitalize ::
  forall a.
  String ->
  StarkProps a
autoCapitalize = PrimProp <<< P.autoCapitalize

autoCorrect :: forall a. String -> StarkProps a
autoCorrect = PrimProp <<< P.autoCorrect

autoSave :: forall a. String -> StarkProps a
autoSave = PrimProp <<< P.autoSave

color :: forall a. String -> StarkProps a
color = PrimProp <<< P.color

itemProp :: forall a. String -> StarkProps a
itemProp = PrimProp <<< P.itemProp

itemScope :: forall a. Boolean -> StarkProps a
itemScope = PrimProp <<< P.itemScope

itemType :: forall a. String -> StarkProps a
itemType = PrimProp <<< P.itemType

itemID :: forall a. String -> StarkProps a
itemID = PrimProp <<< P.itemID

itemRef :: forall a. String -> StarkProps a
itemRef = PrimProp <<< P.itemRef

results :: forall a. Int -> StarkProps a
results = PrimProp <<< P.results

security :: forall a. String -> StarkProps a
security = PrimProp <<< P.security

unselectable :: forall a. Boolean -> StarkProps a
unselectable = PrimProp <<< P.unselectable

onAnimationStart :: StarkProps SyntheticAnimationEvent
onAnimationStart = Handler P.onAnimationStart

onAnimationEnd :: StarkProps SyntheticAnimationEvent
onAnimationEnd = Handler P.onAnimationEnd

onAnimationIteration :: StarkProps SyntheticAnimationEvent
onAnimationIteration = Handler P.onAnimationIteration

onTransitionEnd :: StarkProps SyntheticTransitionEvent
onTransitionEnd = Handler P.onTransitionEnd

onToggle :: StarkProps SyntheticEvent
onToggle = Handler P.onToggle

onError :: StarkProps SyntheticEvent
onError = Handler P.onError

onLoad :: StarkProps SyntheticEvent
onLoad = Handler P.onLoad

onAbort :: StarkProps SyntheticEvent
onAbort = Handler P.onAbort

onCanPlay :: StarkProps SyntheticEvent
onCanPlay = Handler P.onCanPlay

onCanPlayThrough :: StarkProps SyntheticEvent
onCanPlayThrough = Handler P.onCanPlayThrough

onDurationChange :: StarkProps SyntheticEvent
onDurationChange = Handler P.onDurationChange

onEmptied :: StarkProps SyntheticEvent
onEmptied = Handler P.onEmptied

onEncrypted :: StarkProps SyntheticEvent
onEncrypted = Handler P.onEncrypted

onEnded :: StarkProps SyntheticEvent
onEnded = Handler P.onEnded

onLoadedData :: StarkProps SyntheticEvent
onLoadedData = Handler P.onLoadedData

onLoadedMetadata :: StarkProps SyntheticEvent
onLoadedMetadata = Handler P.onLoadedMetadata

onLoadStart :: StarkProps SyntheticEvent
onLoadStart = Handler P.onLoadStart

onPause :: StarkProps SyntheticEvent
onPause = Handler P.onPause

onPlay :: StarkProps SyntheticEvent
onPlay = Handler P.onPlay

onPlaying :: StarkProps SyntheticEvent
onPlaying = Handler P.onPlaying

onProgress :: StarkProps SyntheticEvent
onProgress = Handler P.onProgress

onRateChange :: StarkProps SyntheticEvent
onRateChange = Handler P.onRateChange

onSeeked :: StarkProps SyntheticEvent
onSeeked = Handler P.onSeeked

onSeeking :: StarkProps SyntheticEvent
onSeeking = Handler P.onSeeking

onStalled :: StarkProps SyntheticEvent
onStalled = Handler P.onStalled

onSuspend :: StarkProps SyntheticEvent
onSuspend = Handler P.onSuspend

onTimeUpdate :: StarkProps SyntheticEvent
onTimeUpdate = Handler P.onTimeUpdate

onVolumeChange :: StarkProps SyntheticEvent
onVolumeChange = Handler P.onVolumeChange

onWaiting :: StarkProps SyntheticEvent
onWaiting = Handler P.onWaiting

onCopy :: StarkProps SyntheticClipboardEvent
onCopy = Handler P.onCopy

onCut :: StarkProps SyntheticClipboardEvent
onCut = Handler P.onCut

onPaste :: StarkProps SyntheticClipboardEvent
onPaste = Handler P.onPaste

onCompositionEnd :: StarkProps SyntheticCompositionEvent
onCompositionEnd = Handler P.onCompositionEnd

onCompositionStart :: StarkProps SyntheticCompositionEvent
onCompositionStart = Handler P.onCompositionStart

onCompositionUpdate :: StarkProps SyntheticCompositionEvent
onCompositionUpdate = Handler P.onCompositionUpdate

onKeyDown :: StarkProps SyntheticKeyboardEvent
onKeyDown = Handler P.onKeyDown

onKeyPress :: StarkProps SyntheticKeyboardEvent
onKeyPress = Handler P.onKeyPress

onKeyUp :: StarkProps SyntheticKeyboardEvent
onKeyUp = Handler P.onKeyUp

onKeyEnter :: StarkProps SyntheticKeyboardEvent
onKeyEnter = filterProp isEnterEvent onKeyDown

onFocus :: StarkProps SyntheticFocusEvent
onFocus = Handler P.onFocus

onBlur :: StarkProps SyntheticFocusEvent
onBlur = Handler P.onBlur

onChange :: StarkProps SyntheticInputEvent
onChange = Handler P.onChange

onInput :: StarkProps SyntheticInputEvent
onInput = Handler P.onInput

onInvalid :: StarkProps SyntheticInputEvent
onInvalid = Handler P.onInvalid

onSubmit :: StarkProps SyntheticInputEvent
onSubmit = Handler P.onSubmit

onClick :: StarkProps SyntheticMouseEvent
onClick = Handler P.onClick

onContextMenu :: StarkProps SyntheticMouseEvent
onContextMenu = Handler P.onContextMenu

onDoubleClick :: StarkProps SyntheticMouseEvent
onDoubleClick = Handler P.onDoubleClick

onDrag :: StarkProps SyntheticMouseEvent
onDrag = Handler P.onDrag

onDragEnd :: StarkProps SyntheticMouseEvent
onDragEnd = Handler P.onDragEnd

onDragEnter :: StarkProps SyntheticMouseEvent
onDragEnter = Handler P.onDragEnter

onDragExit :: StarkProps SyntheticMouseEvent
onDragExit = Handler P.onDragExit

onDragLeave :: StarkProps SyntheticMouseEvent
onDragLeave = Handler P.onDragLeave

onDragOver :: StarkProps SyntheticMouseEvent
onDragOver = Handler P.onDragOver

onDragStart :: StarkProps SyntheticMouseEvent
onDragStart = Handler P.onDragStart

onDrop :: StarkProps SyntheticMouseEvent
onDrop = Handler P.onDrop

onMouseDown :: StarkProps SyntheticMouseEvent
onMouseDown = Handler P.onMouseDown

onMouseEnter :: StarkProps SyntheticMouseEvent
onMouseEnter = Handler P.onMouseEnter

onMouseLeave :: StarkProps SyntheticMouseEvent
onMouseLeave = Handler P.onMouseLeave

onMouseMove :: StarkProps SyntheticMouseEvent
onMouseMove = Handler P.onMouseMove

onMouseOut :: StarkProps SyntheticMouseEvent
onMouseOut = Handler P.onMouseOut

onMouseOver :: StarkProps SyntheticMouseEvent
onMouseOver = Handler P.onMouseOver

onMouseUp :: StarkProps SyntheticMouseEvent
onMouseUp = Handler P.onMouseUp

onSelect :: StarkProps SyntheticEvent
onSelect = Handler P.onSelect

onTouchCancel :: StarkProps SyntheticTouchEvent
onTouchCancel = Handler P.onTouchCancel

onTouchEnd :: StarkProps SyntheticTouchEvent
onTouchEnd = Handler P.onTouchEnd

onTouchMove :: StarkProps SyntheticTouchEvent
onTouchMove = Handler P.onTouchMove

onTouchStart :: StarkProps SyntheticTouchEvent
onTouchStart = Handler P.onTouchStart

onScroll :: StarkProps SyntheticUIEvent
onScroll = Handler P.onScroll

onWheel :: StarkProps SyntheticWheelEvent
onWheel = Handler P.onWheel

onAnimationStartCapture :: StarkProps SyntheticAnimationEvent
onAnimationStartCapture = Handler P.onAnimationStartCapture

onAnimationEndCapture :: StarkProps SyntheticAnimationEvent
onAnimationEndCapture = Handler P.onAnimationEndCapture

onAnimationIterationCapture :: StarkProps SyntheticAnimationEvent
onAnimationIterationCapture = Handler P.onAnimationIterationCapture

onTransitionEndCapture :: StarkProps SyntheticTransitionEvent
onTransitionEndCapture = Handler P.onTransitionEndCapture

onToggleCapture :: StarkProps SyntheticEvent
onToggleCapture = Handler P.onToggleCapture

onErrorCapture :: StarkProps SyntheticEvent
onErrorCapture = Handler P.onErrorCapture

onLoadCapture :: StarkProps SyntheticEvent
onLoadCapture = Handler P.onLoadCapture

onAbortCapture :: StarkProps SyntheticEvent
onAbortCapture = Handler P.onAbortCapture

onCanPlayCapture :: StarkProps SyntheticEvent
onCanPlayCapture = Handler P.onCanPlayCapture

onCanPlayThroughCapture :: StarkProps SyntheticEvent
onCanPlayThroughCapture = Handler P.onCanPlayThroughCapture

onDurationChangeCapture :: StarkProps SyntheticEvent
onDurationChangeCapture = Handler P.onDurationChangeCapture

onEmptiedCapture :: StarkProps SyntheticEvent
onEmptiedCapture = Handler P.onEmptiedCapture

onEncryptedCapture :: StarkProps SyntheticEvent
onEncryptedCapture = Handler P.onEncryptedCapture

onEndedCapture :: StarkProps SyntheticEvent
onEndedCapture = Handler P.onEndedCapture

onLoadedDataCapture :: StarkProps SyntheticEvent
onLoadedDataCapture = Handler P.onLoadedDataCapture

onLoadedMetadataCapture :: StarkProps SyntheticEvent
onLoadedMetadataCapture = Handler P.onLoadedMetadataCapture

onLoadStartCapture :: StarkProps SyntheticEvent
onLoadStartCapture = Handler P.onLoadStartCapture

onPauseCapture :: StarkProps SyntheticEvent
onPauseCapture = Handler P.onPauseCapture

onPlayCapture :: StarkProps SyntheticEvent
onPlayCapture = Handler P.onPlayCapture

onPlayingCapture :: StarkProps SyntheticEvent
onPlayingCapture = Handler P.onPlayingCapture

onProgressCapture :: StarkProps SyntheticEvent
onProgressCapture = Handler P.onProgressCapture

onRateChangeCapture :: StarkProps SyntheticEvent
onRateChangeCapture = Handler P.onRateChangeCapture

onSeekedCapture :: StarkProps SyntheticEvent
onSeekedCapture = Handler P.onSeekedCapture

onSeekingCapture :: StarkProps SyntheticEvent
onSeekingCapture = Handler P.onSeekingCapture

onStalledCapture :: StarkProps SyntheticEvent
onStalledCapture = Handler P.onStalledCapture

onSuspendCapture :: StarkProps SyntheticEvent
onSuspendCapture = Handler P.onSuspendCapture

onTimeUpdateCapture :: StarkProps SyntheticEvent
onTimeUpdateCapture = Handler P.onTimeUpdateCapture

onVolumeChangeCapture :: StarkProps SyntheticEvent
onVolumeChangeCapture = Handler P.onVolumeChangeCapture

onWaitingCapture :: StarkProps SyntheticEvent
onWaitingCapture = Handler P.onWaitingCapture

onCopyCapture :: StarkProps SyntheticClipboardEvent
onCopyCapture = Handler P.onCopyCapture

onCutCapture :: StarkProps SyntheticClipboardEvent
onCutCapture = Handler P.onCutCapture

onPasteCapture :: StarkProps SyntheticClipboardEvent
onPasteCapture = Handler P.onPasteCapture

onCompositionEndCapture :: StarkProps SyntheticCompositionEvent
onCompositionEndCapture = Handler P.onCompositionEndCapture

onCompositionStartCapture :: StarkProps SyntheticCompositionEvent
onCompositionStartCapture = Handler P.onCompositionStartCapture

onCompositionUpdateCapture :: StarkProps SyntheticCompositionEvent
onCompositionUpdateCapture = Handler P.onCompositionUpdateCapture

onKeyDownCapture :: StarkProps SyntheticKeyboardEvent
onKeyDownCapture = Handler P.onKeyDownCapture

onKeyPressCapture :: StarkProps SyntheticKeyboardEvent
onKeyPressCapture = Handler P.onKeyPressCapture

onKeyUpCapture :: StarkProps SyntheticKeyboardEvent
onKeyUpCapture = Handler P.onKeyUpCapture

onFocusCapture :: StarkProps SyntheticFocusEvent
onFocusCapture = Handler P.onFocusCapture

onBlurCapture :: StarkProps SyntheticFocusEvent
onBlurCapture = Handler P.onBlurCapture

onChangeCapture :: StarkProps SyntheticInputEvent
onChangeCapture = Handler P.onChangeCapture

onInputCapture :: StarkProps SyntheticInputEvent
onInputCapture = Handler P.onInputCapture

onInvalidCapture :: StarkProps SyntheticInputEvent
onInvalidCapture = Handler P.onInvalidCapture

onSubmitCapture :: StarkProps SyntheticInputEvent
onSubmitCapture = Handler P.onSubmitCapture

onClickCapture :: StarkProps SyntheticMouseEvent
onClickCapture = Handler P.onClickCapture

onContextMenuCapture :: StarkProps SyntheticMouseEvent
onContextMenuCapture = Handler P.onContextMenuCapture

onDoubleClickCapture :: StarkProps SyntheticMouseEvent
onDoubleClickCapture = Handler P.onDoubleClickCapture

onDragCapture :: StarkProps SyntheticMouseEvent
onDragCapture = Handler P.onDragCapture

onDragEndCapture :: StarkProps SyntheticMouseEvent
onDragEndCapture = Handler P.onDragEndCapture

onDragEnterCapture :: StarkProps SyntheticMouseEvent
onDragEnterCapture = Handler P.onDragEnterCapture

onDragExitCapture :: StarkProps SyntheticMouseEvent
onDragExitCapture = Handler P.onDragExitCapture

onDragLeaveCapture :: StarkProps SyntheticMouseEvent
onDragLeaveCapture = Handler P.onDragLeaveCapture

onDragOverCapture :: StarkProps SyntheticMouseEvent
onDragOverCapture = Handler P.onDragOverCapture

onDragStartCapture :: StarkProps SyntheticMouseEvent
onDragStartCapture = Handler P.onDragStartCapture

onDropCapture :: StarkProps SyntheticMouseEvent
onDropCapture = Handler P.onDropCapture

onMouseDownCapture :: StarkProps SyntheticMouseEvent
onMouseDownCapture = Handler P.onMouseDownCapture

onMouseEnterCapture :: StarkProps SyntheticMouseEvent
onMouseEnterCapture = Handler P.onMouseEnterCapture

onMouseLeaveCapture :: StarkProps SyntheticMouseEvent
onMouseLeaveCapture = Handler P.onMouseLeaveCapture

onMouseMoveCapture :: StarkProps SyntheticMouseEvent
onMouseMoveCapture = Handler P.onMouseMoveCapture

onMouseOutCapture :: StarkProps SyntheticMouseEvent
onMouseOutCapture = Handler P.onMouseOutCapture

onMouseOverCapture :: StarkProps SyntheticMouseEvent
onMouseOverCapture = Handler P.onMouseOverCapture

onMouseUpCapture :: StarkProps SyntheticMouseEvent
onMouseUpCapture = Handler P.onMouseUpCapture

onSelectCapture :: StarkProps SyntheticEvent
onSelectCapture = Handler P.onSelectCapture

onTouchCancelCapture :: StarkProps SyntheticTouchEvent
onTouchCancelCapture = Handler P.onTouchCancelCapture

onTouchEndCapture :: StarkProps SyntheticTouchEvent
onTouchEndCapture = Handler P.onTouchEndCapture

onTouchMoveCapture :: StarkProps SyntheticTouchEvent
onTouchMoveCapture = Handler P.onTouchMoveCapture

onTouchStartCapture :: StarkProps SyntheticTouchEvent
onTouchStartCapture = Handler P.onTouchStartCapture

onScrollCapture :: StarkProps SyntheticUIEvent
onScrollCapture = Handler P.onScrollCapture

onWheelCapture :: StarkProps SyntheticWheelEvent
onWheelCapture = Handler P.onWheelCapture

ref :: forall a. Ref.RefHandler Ref.NativeNode -> StarkProps a
ref = PrimProp <<< P.ref

suppressContentEditableWarning :: forall a. Boolean -> StarkProps a
suppressContentEditableWarning = PrimProp <<< P.suppressContentEditableWarning

-- SVG attributes
x ::
  forall a.
  Int ->
  StarkProps a
x = PrimProp <<< P.x

y :: forall a. Int -> StarkProps a
y = PrimProp <<< P.y

cx :: forall a. Int -> StarkProps a
cx = PrimProp <<< P.cx

cy :: forall a. Int -> StarkProps a
cy = PrimProp <<< P.cy

r :: forall a. Int -> StarkProps a
r = PrimProp <<< P.r

fill :: forall a. String -> StarkProps a
fill = PrimProp <<< P.fill

opacity :: forall a. Int -> StarkProps a
opacity = PrimProp <<< P.opacity

fillOpacity :: forall a. Int -> StarkProps a
fillOpacity = PrimProp <<< P.fillOpacity

stroke :: forall a. String -> StarkProps a
stroke = PrimProp <<< P.stroke

strokeWidth :: forall a. Int -> StarkProps a
strokeWidth = PrimProp <<< P.strokeWidth

points :: forall a. String -> StarkProps a
points = PrimProp <<< P.points

d :: forall a. String -> StarkProps a
d = PrimProp <<< P.d

viewBox :: forall a. String -> StarkProps a
viewBox = PrimProp <<< P.viewBox
