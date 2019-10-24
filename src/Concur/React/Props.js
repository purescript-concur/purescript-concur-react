"use strict";

exports.resetTargetValue = function(s) {
    return function(event) {
        return function() {
            event.target.value = s;
        }
    };
};

exports.emptyProp_ = {}
