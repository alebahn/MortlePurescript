module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Graphics.Canvas (getCanvasElementById, getContext2D, setFont, fillText)

main :: Effect Unit
main = do
  maybeCanvasElement <- getCanvasElementById "gameCanvas"
  case maybeCanvasElement of
    Nothing -> do
      log "Nothing"
    Just canvasElement -> do
      context <- getContext2D canvasElement
      setFont context "8px 'Press Start 2P'"
      fillText context "MORTLE" 24.0 12.0
      --this.canvasContext.fillText("MORTLE", 24, 12);
      log "🍝"
