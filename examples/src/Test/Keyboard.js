"use strict";

var counter = 0;
var observers = [];

function innerHandler(event) {
  event.preventDefault();
  observers.forEach(function(o){o(event);});
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
  var id = counter++;
  observers[id] = onSuccess;
  return function (cancelError, onCancelerError, onCancelerSuccess) {
    delete observers[id];
    onCancelerSuccess();
  };
};
