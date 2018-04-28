"use strict";

exports.getEventTargetValueString = function(event) {
    return event.target.value;
};

exports.getKeyboardEventKeyString = function(event) {
    return event.key;
};

exports.resetTargetValue = function(s) {
    return function(event) {
        return function() {
            event.target.value = s;
        }
    };
};
