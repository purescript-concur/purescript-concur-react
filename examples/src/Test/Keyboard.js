"use strict";

var observer = null;

function innerHandler(event) {
  event.preventDefault();
  if(observer) observer(event);
}

// Start listening for keys
// :: Effect Unit
exports.startListening = function() {
  document.addEventListener('keydown', innerHandler);
};

// Stop listening for keys
// :: Effect Unit
exports.stopListening = function() {
  document.removeEventListener('keydown', innerHandler);
};

// Await a key
// :: EffectFnAff KeyEvent
exports._awaitKey = function (onError, onSuccess) {
  observer = onSuccess;
  return function (cancelError, onCancelerError, onCancelerSuccess) {
    onCancelerSuccess();
  };
};
