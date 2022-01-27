"use strict";

exports.mkMainLike = function(f) {
    return function() {
        return f();
    };
};
