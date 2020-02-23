"use strict"

exports.setCanvasBackgroundURL = function (canvasElement) {
  return function (url) {
    return function () {
      canvasElement.style.background = "url(" + url + ")";
    }
  }
}