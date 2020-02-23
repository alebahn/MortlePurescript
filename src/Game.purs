module Game where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasElement, Context2D, canvasToDataURL, clearRect, fillText, getCanvasDimensions, getContext2D, setFont)
import Signal (Signal, runSignal, (~>), filter, merge, foldp)
import Signal.DOM (animationFrame, keyPressed)

foreign import setCanvasBackgroundURL :: CanvasElement -> String -> Effect Unit

data GameState = Startup | Menu | Aim | Launch | Win
derive instance eqGameState :: Eq GameState

data MenuOption = NewGame | Continue

type GameModel = {
  canvasElement :: CanvasElement,
  canvasContext :: Context2D,
  width :: Number,
  height :: Number,
  currentState :: GameState,
  nextState :: GameState,
  menuOption :: MenuOption,
  refreshScreen :: Boolean,
  paintScreen :: Boolean
}

data Input = KeyLeft | KeyRight | KeyUp | KeyDown | KeyOther | Frame

clearScreen :: GameModel -> Effect Unit
clearScreen game = do
  let rectangle = {x: 0.0, y: 0.0, width: game.width, height: game.height}
  clearRect game.canvasContext rectangle

saveToBackground :: GameModel -> Effect Unit
saveToBackground game = do
  url <- canvasToDataURL game.canvasElement
  setCanvasBackgroundURL game.canvasElement url

drawMenu :: GameModel -> Effect Unit
drawMenu game = do
  let context = game.canvasContext
  setFont context "8px 'Press Start 2P'"
  fillText context "MORTLE" 24.0 12.0
  fillText context "New Game" 4.0 24.0
  fillText context "Continue" 4.0 32.0

menu :: GameModel -> Effect Unit
menu game = do
  clearScreen game
  drawMenu game
  saveToBackground game
  clearScreen game

makeGame :: CanvasElement -> Context2D -> Number -> Number -> GameModel
makeGame canvasElement context width height = {
  canvasElement: canvasElement,
  canvasContext: context,
  width: width,
  height: height,
  currentState: Startup,
  nextState: Menu,
  menuOption: NewGame,
  refreshScreen: false,
  paintScreen: false
}

getInputFromKeycode :: Int -> Input
getInputFromKeycode keyCode = case keyCode of
  37 -> KeyLeft
  38 -> KeyUp
  39 -> KeyRight
  40 -> KeyDown
  _ -> KeyOther

getKeyInput :: Int -> Effect (Signal Input)
getKeyInput keyCode = do
  let input = getInputFromKeycode keyCode
  keypress <- keyPressed keyCode
  let keyDowns = filter identity true keypress
  pure (keyDowns ~> (\_ -> input ))

getAnimationFrameInput :: Effect (Signal Input)
getAnimationFrameInput = do
  animationFrames <- animationFrame
  pure (animationFrames ~> (\_ -> Frame))

getInputSignal :: Effect (Signal Input)
getInputSignal = do
  signalLeft <- getKeyInput 37
  signalUp <- getKeyInput 38
  signalRight <- getKeyInput 39
  signalDown <- getKeyInput 40
  signalEnter <- getKeyInput 13
  signalSpace <- getKeyInput 32
  signalFrame <- getAnimationFrameInput
  pure $ merge signalLeft $ merge signalUp $ merge signalRight $ merge signalDown $ merge signalEnter $ merge signalSpace signalFrame

update :: Input -> GameModel -> GameModel
update Frame game =
  let
    refreshScreen = game.currentState == game.nextState
  in
    let
      game' = game {
        refreshScreen = refreshScreen,
        currentState = game.nextState
        }
    in
      game' {
        paintScreen = true
      }
update input game = game { paintScreen = false}

getYCoordinateFromMenuOption :: MenuOption -> Number
getYCoordinateFromMenuOption NewGame = 24.0
getYCoordinateFromMenuOption Continue = 32.0

display :: GameModel -> Effect Unit
display game | game.paintScreen = do
  if game.refreshScreen
    then case game.currentState of
      Menu -> menu game
      _ -> pure unit
    else pure unit
  case game.currentState of
    Menu -> do
      let xCoordinate = 72.0
      clearScreen game
      let yCoordinate = getYCoordinateFromMenuOption game.menuOption
      fillText game.canvasContext "<" xCoordinate yCoordinate
    _ -> pure unit
display game = pure unit  --ignore if no paint flag

mainLoop :: GameModel -> Effect Unit
mainLoop game = do
  inputSignal <- getInputSignal
  let displaySignal = foldp update game inputSignal
  runSignal (displaySignal ~> display)

start :: CanvasElement -> Effect Unit
start canvasElement = do
  context <- getContext2D canvasElement
  dimensions <- getCanvasDimensions canvasElement
  let game = makeGame canvasElement context dimensions.width dimensions.height
  mainLoop game