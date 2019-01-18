"use strict";

var DataMaybe = require('../Data.Maybe');

// Global cache doesn't reset with hot-reload
function getCache(label) {
  if(!window.__concur_checkpoints_cache__) {
    console.log("Creating cache ");
    window.__concur_checkpoints_cache__ = {};
  }
  // console.log("Cache is ", window.__concur_checkpoints_cache__);
  var cache = window.__concur_checkpoints_cache__[label];
  if(!cache) {
    cache = window.__concur_checkpoints_cache__[label] = {};
  }
  return cache;
}

// This will reset with hot-reload
window.__concur_process_index__ = {};

function getProcessIndex(label) {
  debugger;
  console.log("Process Index is ", window.__concur_process_index__[label]);
  var curr = window.__concur_process_index__[label];
  if(!curr) return 0;
  else return curr;
};

function getAndIncrProcessIndex(label) {
  debugger;
  var curr = window.__concur_process_index__[label];
  if(!curr) curr = 0;
  window.__concur_process_index__[label] = curr + 1;
  return curr;
};

// API
exports.incrProcessIndex = function(label) {
  return function() {
    debugger;
    var curr = window.__concur_process_index__[label];
    if(!curr) curr = 0;
    window.__concur_process_index__[label] = curr + 1;
  };
};

exports.readCheckpoint = function(label) {
  return function() {
    debugger;
    var cache = getCache(label);
    var curr = getProcessIndex(label);
    if(cache.hasOwnProperty(curr)) {
      return DataMaybe.Just.create(cache[curr]);
    } else {
      return DataMaybe.Nothing.value;
    }
  };
};

exports.writeCheckpoint = function(label) {
  return function(val) {
    return function() {
      debugger;
      var cache = getCache(label);
      var curr = getAndIncrProcessIndex(label);
      cache[curr] = val;
    };
  };
};