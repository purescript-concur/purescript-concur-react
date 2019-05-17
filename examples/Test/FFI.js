"use strict";

// Local storage functions

exports.storageLength = function() {
  return window.localStorage.length;
};

exports.storageGet = function(key) {
  return function() {
    return window.localStorage.getItem(key);
  };
};

exports.storageSet = function(key) {
  return function(val) {
    return function() {
      window.localStorage.setItem(key, val);
    };
  };
};

exports.storageDelete = function(key) {
  return function() {
    window.localStorage.removeItem(key);
  };
};

exports.storageClear = function() {
  window.localStorage.clear();
};
