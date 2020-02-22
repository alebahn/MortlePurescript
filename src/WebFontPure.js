"use strict";

exports.load = function (config) {
  return function () {
    WebFont.load(config);
  }
}