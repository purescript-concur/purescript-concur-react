"use strict";

var observer = null;

function innerHandler(event) {
  event.preventDefault();
  if(observer) observer(event);
}

// Start listening for keys
// :: Effect Unit
export function startListening () {
  document.addEventListener('keydown', innerHandler);
}

// Stop listening for keys
// :: Effect Unit
export function stopListening () {
  document.removeEventListener('keydown', innerHandler);
}

// Await a key
// :: EffectFnAff KeyEvent
export function _awaitKey (onError, onSuccess) {
  observer = onSuccess;
  return function (cancelError, onCancelerError, onCancelerSuccess) {
    onCancelerSuccess();
  };
}
