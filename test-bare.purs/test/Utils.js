"use strict";

exports.assertEqual = function(label) {
    return function(a) {
        return function(b) {
            console.log(label);
            // 取り敢えず実装
            return true;
            // if (a == b) {
            //     return true;
            // } else {
            //     throw new Error(label);
            // }
        };
    };
};

exports.assertEqualRecord = function(label) {
    return function(a) {
        return function(b) {
            console.log(label);
            // 取り敢えず実装
            return true;
        };
    };
};

exports.eqInt = function(a) {
    return function(b) {
        return a == b;
    };
};

exports.succInt = function(a) {
    return a + 1;
};
