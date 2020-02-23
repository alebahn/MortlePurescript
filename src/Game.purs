module Game where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Graphics.Canvas (CanvasElement, Context2D, canvasToDataURL, clearRect, fillText, getCanvasDimensions, getContext2D, setFont)

foreign import setCanvasBackgroundURL :: CanvasElement -> String -> Effect Unit

data GameState = Startup | Menu | Aim | Launch | Win

data MenuOption = NewGame | Continue

type Game= {
  canvasElement :: CanvasElement,
  canvasContext :: Context2D,
  width :: Number,
  height :: Number
}

makeGame :: CanvasElement -> Context2D -> Number -> Number -> Game
makeGame canvasElement context width height = {
  canvasElement: canvasElement,
  canvasContext: context,
  width: width,
  height: height
  }

clearScreen :: Game -> Effect Unit
clearScreen game = do
  let rectangle = {x: 0.0, y: 0.0, width: game.width, height: game.height}
  clearRect game.canvasContext rectangle

saveToBackground :: Game -> Effect Unit
saveToBackground game = do
  url <- canvasToDataURL game.canvasElement
  setCanvasBackgroundURL game.canvasElement url

drawMenu :: Game -> Effect Unit
drawMenu game = do
  let context = game.canvasContext
  setFont context "8px 'Press Start 2P'"
  fillText context "MORTLE" 24.0 12.0
  fillText context "New Game" 4.0 24.0
  fillText context "Continue" 4.0 32.0

menuLoop :: Game -> Aff GameState
menuLoop game = pure Aim

menu :: Game -> Aff GameState
menu game = do
  liftEffect do
    clearScreen game
    drawMenu game
    saveToBackground game
    clearScreen game
  menuLoop game

mainLoop :: Game -> Aff Unit
mainLoop game = do
  _ <- menu game
  pure unit
  --mainLoop game

start :: CanvasElement -> Aff Unit
start canvasElement = do
  context <- liftEffect $ getContext2D canvasElement
  dimensions <- liftEffect $ getCanvasDimensions canvasElement
  let game = makeGame canvasElement context dimensions.width dimensions.height
  mainLoop game