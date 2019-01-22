"use strict";

exports.resetTargetValue = function(s) {
    return function(event) {
        return function() {
            event.target.value = s;
        }
    };
};

exports.emptyProp_ = {}

exports.createRef = function() {
  return (function() {
    var x;
    return [
      function(cur) {
        x=cur;
      },
      function() {
        return x;
      }
    ];
  })();
};

exports.refSetter = function(ref) {
  return function(cur) {
    return function() {
      return ref[0](cur);
    }
  };
};

exports.refGetter_ = function(ref) {
  return function() {
    return ref[1]();
  };
};
