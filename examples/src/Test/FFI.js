"use strict";

// Local storage functions

export function storageLength () {
  return window.localStorage.length;
}

export function storageGet (key) {
  return function() {
    return window.localStorage.getItem(key);
  };
}

export function storageSet (key) {
  return function(val) {
    return function() {
      window.localStorage.setItem(key, val);
    };
  };
}

export function storageDelete (key) {
  return function() {
    window.localStorage.removeItem(key);
  };
}

export function storageClear () {
  window.localStorage.clear();
}

// Keyboard events. Allows multiple key events at the same time
export function handleKeyboardEvents (handler) {
  var keys = {};
  var keyUpHandler = function(event){
    const keyName = event.key;
    delete keys[keyName];
    handler(Object.keys(keys))();
  };
  var keyDownHandler = function(event){
    const keyName = event.key;
    keys[keyName] = 1;
    handler(Object.keys(keys))();
  };
  return function() {
      document.addEventListener('keydown', keyDownHandler);
      document.addEventListener('keyup', keyUpHandler);
      return function() {
        document.removeEventListener('keydown', keyDownHandler);
        document.removeEventListener('keyup', keyUpHandler);
      };
  };
}

// Time interval events
export function setTimeInterval (handler) {
  return function(time) {
    return function() {
      setinterval(handler, time);
      return function() {
        clearInterval(handler, time);
      };
    };
  };
}
