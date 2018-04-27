"use strict";

exports.getEventTargetValueString = function(event) {
    return event.target.value;
};

exports.getKeyboardEventKeyString = function(event) {
    return event.key;
};
