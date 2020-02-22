module Game where

import Prelude

import Graphics.Canvas (CanvasElement, getContext2D, setFont, fillText)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

start :: CanvasElement -> Aff Unit
start canvasElement = do
  liftEffect do
    context <- getContext2D canvasElement
    setFont context "8px 'Press Start 2P'"
    fillText context "MORTLE" 24.0 12.0

--loop :: GameState -> Aff GameState
--loop gameState = pure GameState
