"use strict";

export const resetTargetValue = function(s) {
    return function(event) {
        return function() {
            event.target.value = s;
        }
    };
};

export const emptyProp_ = {}
