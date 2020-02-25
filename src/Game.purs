module Game where

import Prelude

import Data.Array (mapWithIndex, (!!))
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect, foreachE)
import Effect.Class.Console (log)
import Graphics.Canvas (CanvasElement, Context2D, canvasToDataURL, clearRect, fillRect, fillText, getCanvasDimensions, getContext2D, setFillStyle, setFont, strokeRect)
import Math (abs, floor, round, sin, cos, pi)
import Signal (Signal, runSignal, (~>), filter, merge, foldp)
import Signal.DOM (animationFrame, keyPressed)

foreign import setCanvasBackgroundURL :: CanvasElement -> String -> Effect Unit

o :: Boolean
o = false
i :: Boolean
i = true

levels :: Array (Array (Array Boolean))
levels = [
  [
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o],
    [o,o,o,i,i,i,i,i,i,i,i,i,i,i,i,i,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o]
  ],
  [
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,i,i,i,i,i,o,i,i,i,i,i,i,i,o,i,i],
    [o,o,i,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o],
    [o,i,i,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o]
  ],
  [
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,i,i,i,i,i,o,o,o],
    [o,i,i,o,o,o,o,o,o,o,o,o,i,o,i,o,o,o,o],
    [o,o,i,i,o,o,o,o,o,o,o,o,i,o,i,o,o,o,o],
    [i,o,i,o,i,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,i,o,o,i,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,i,i,o,o,o,i,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,i,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o,o],
    [i,o,i,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o],
    [o,o,i,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o],
    [o,i,i,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o],
    [o,o,i,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o]
  ],
  [
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,i,i,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,i,i,o,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,i,i,o,o,o,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,i,o,i,o,i,o,i,o,o,o,o,o,o],
    [o,o,o,i,i,o,i,o,i,o,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,i,i,i,o,i,o,i,o,o,o,o,o,o],
    [o,i,i,o,o,o,i,o,i,o,i,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o,o,o,o]
  ],
  [
    [o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o,o],
    [i,i,i,o,o,o,o,i,o,o,o,o,i,i,o,i,o,o,o],
    [o,o,o,o,o,o,o,i,o,o,o,o,i,o,o,i,o,o,o],
    [o,o,o,i,i,i,o,o,o,o,o,o,i,o,i,i,o,o,o],
    [o,o,o,o,o,i,o,o,o,o,o,o,i,o,o,i,o,o,o],
    [o,i,i,o,o,i,i,i,i,i,i,o,i,i,o,i,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,i,o,i,o,o,i,o,o,o],
    [o,o,o,o,i,i,o,o,o,o,i,o,i,o,i,i,o,o,o],
    [o,o,o,o,i,o,o,o,o,o,i,o,o,o,o,i,o,o,o],
    [o,o,o,o,i,o,i,i,i,o,i,i,i,i,o,i,o,o,o],
    [o,i,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,i,i],
    [o,i,o,o,i,i,o,o,o,o,o,o,o,o,o,i,o,o,o]
  ],
  [
    [i,i,i,i,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,i,o,i,i,o,o,o,o,o,o,o,o,o,o,o,o],
    [i,i,o,i,o,o,o,o,o,i,i,o,o,o,o,o,o,o,o],
    [o,o,o,i,o,o,o,o,o,i,o,o,o,i,i,o,o,o,o],
    [o,i,i,i,i,i,i,o,o,i,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,i,o,o,i,o,o,o,o,o,o],
    [i,i,i,o,o,o,o,o,o,i,o,o,i,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,i,o,i,o,o,i,o,o,o,o,o,o],
    [o,o,o,i,i,i,i,o,o,i,o,o,o,o,o,i,o,i,i],
    [i,i,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o,o]
  ],
  [
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,i,i,i,i,i,i,i,i,i,i,o],
    [o,o,o,o,o,o,o,i,i,o,o,o,o,o,o,o,o,o,o],
    [o,i,i,i,i,i,i,i,i,o,o,i,i,i,i,i,i,i,i],
    [o,o,o,o,o,o,o,o,i,o,o,i,o,o,o,o,o,o,o],
    [i,o,o,o,o,o,o,i,i,o,o,i,o,o,o,i,o,o,o],
    [i,i,i,i,i,i,o,o,i,o,i,i,i,i,o,i,o,o,o],
    [o,o,o,o,o,i,i,o,i,o,o,i,o,o,o,i,o,o,o],
    [o,o,o,o,o,i,o,o,i,o,o,i,o,i,i,i,o,o,o],
    [o,o,i,i,o,i,o,i,i,o,o,i,o,o,o,i,o,o,o],
    [i,o,i,o,o,o,o,o,i,i,o,i,i,i,o,i,o,o,o],
    [o,o,i,o,o,o,i,o,i,o,o,o,o,o,o,i,o,o,o]
  ],
  [
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],
    [o,o,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,o,i],
    [o,o,o,i,o,o,o,o,o,o,o,o,o,o,o,o,i,o,o],
    [i,i,o,i,o,i,i,i,i,i,i,i,i,i,o,o,i,o,o],
    [o,o,o,i,o,i,o,o,o,o,o,o,o,i,i,o,i,i,o],
    [o,i,o,i,o,i,i,o,i,i,i,i,o,i,o,o,o,i,o],
    [o,o,o,i,o,i,o,o,i,o,o,o,o,i,i,o,o,i,o],
    [o,i,i,i,o,i,o,i,i,o,i,o,o,i,o,o,o,o,o],
    [o,o,o,i,o,i,o,o,i,o,i,i,i,i,i,i,i,i,i],
    [i,o,o,i,o,i,o,i,i,o,o,o,o,o,o,o,o,o,o],
    [o,o,o,i,o,o,o,o,i,o,o,o,o,o,o,o,o,o,o]
  ],
  [
    [i,i,o,i,o,i,i,i,i,i,o,i,o,i,i,i,i,i,o],
    [o,o,o,i,o,o,o,o,o,o,o,i,o,o,o,o,o,o,o],
    [o,i,i,i,i,i,o,i,o,i,i,i,i,i,o,i,o,i,i],
    [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
    [i,i,o,i,o,i,i,i,i,i,o,i,o,i,i,i,i,i,o],
    [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
    [o,i,i,i,i,i,o,i,o,i,i,i,i,i,o,i,o,i,i],
    [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
    [i,i,o,i,o,i,i,i,i,i,o,i,o,i,i,i,i,i,o],
    [o,o,o,i,o,o,o,i,o,o,o,i,o,o,o,i,o,o,o],
    [o,i,i,i,i,i,o,i,o,i,i,i,i,i,o,i,o,i,i],
    [o,o,o,o,o,o,o,i,o,o,o,o,o,o,o,i,o,o,o]
  ],
  [
    [i,o,o,i,o,o,o,o,o,i,o,o,o,o,o,i,o,o,i],
    [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
    [o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o],
    [i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i],
    [o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o],
    [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
    [i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,o],
    [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
    [o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o],
    [i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i],
    [o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o,o,i,o],
    [o,i,o,o,o,o,o,i,o,o,o,o,o,i,o,o,o,o,o]
  ]
]

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
  paintScreen :: Boolean,
  currentLevel :: Int,
  nextLevel :: Int,
  currentX :: Number,
  currentY :: Number,
  aimAngle :: Number,
  velocityX :: Number,
  velocityY :: Number
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
  paintScreen: false,
  currentLevel: -1,
  nextLevel: -1,
  currentX: 2.0,
  currentY: 58.0,
  aimAngle: 0.0,
  velocityX: 0.0,
  velocityY: 0.0
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

angleDelta :: Number
angleDelta = pi / 32.0

update :: Input -> GameModel -> GameModel
update Frame game = --handle animation frame
  let
    refreshScreen = if game.currentState /= game.nextState
      then case game.nextState of
        Menu -> true
        Aim -> game.currentLevel /= game.nextLevel
        _ -> false
      else false
  in
    let
      game' = game {
        paintScreen = true,
        refreshScreen = refreshScreen,
        currentState = game.nextState,
        currentLevel = game.nextLevel
        }
    in
      game'
update input game = --handle keypress
  let
    game' = game {
      paintScreen = false
    }
  in
    case game.currentState of
      Menu -> case input of
        KeyUp -> game' { menuOption = NewGame }
        KeyDown -> game' { menuOption = Continue }
        KeyLeft -> game' -- do nothing
        _ -> game' {nextLevel = 0, nextState = Aim}
      Aim -> case input of
        KeyLeft -> game' { aimAngle = game.aimAngle - angleDelta }
        KeyRight -> game' { aimAngle = game.aimAngle + angleDelta }
        _ -> game' {nextState = Launch, velocityX = sin game.aimAngle, velocityY = -cos game.aimAngle}
      _ -> game'

drawCell :: GameModel -> Int -> Int -> Boolean -> Effect Unit
drawCell game y x cell = if cell
  then
    let rectangle = {x: ((toNumber x) * 5.0), y: ((toNumber y) * 5.0), width: 5.0, height: 5.0}
    in
      fillRect game.canvasContext rectangle
  else
    pure unit

drawRow :: GameModel -> Int -> Array Boolean -> Effect Unit
drawRow game y row = foreachE (mapWithIndex (drawCell game y) row) identity

drawPixel :: GameModel -> Number -> Number -> Effect Unit
drawPixel game x y = do
  let rectangle = {x: x, y: y, width: 1.0, height: 1.0}
  fillRect game.canvasContext rectangle
    --this.canvasContext.fillRect(93, 57, 1, 1);

drawDoor :: GameModel -> Effect Unit
drawDoor game = do
  let rectangle = {x: 90.5, y: 55.5, width: 4.0, height: 4.0}
  strokeRect game.canvasContext rectangle
  drawPixel game 93.0 57.0

drawLevel :: GameModel -> Effect Unit
drawLevel game = do
  let maybeLevelData = levels !! game.currentLevel
  case maybeLevelData of
    Nothing -> log ("Can't find level " <> (show game.currentLevel))
    Just levelData -> do
      foreachE (mapWithIndex (drawRow game) levelData) identity
      drawDoor game

level :: GameModel -> Effect Unit
level game = do
  clearScreen game
  drawLevel game
  saveToBackground game
  clearScreen game

getYCoordinateFromMenuOption :: MenuOption -> Number
getYCoordinateFromMenuOption NewGame = 24.0
getYCoordinateFromMenuOption Continue = 32.0

aimLength :: Number
aimLength = 6.0

checkCollision :: GameModel -> Number -> Number -> Boolean
checkCollision game x y = case (do
  levelData <- levels !! game.currentLevel
  let x' = round x
  let y' = round y

  rowInt <- fromNumber $ floor (x' / 5.0)
  colInt <- fromNumber $ floor (y' / 5.0)
  row <-levelData !! rowInt
  cell <- row !! colInt
  pure cell) of
    Nothing -> true
    Just result -> result

invertPixel :: GameModel -> Number -> Number -> Effect Unit
invertPixel game x y = let
    rectangle = {x: x, y: y, width: 1.0, height: 1.0}
    context = game.canvasContext
  in
    if checkCollision game x y
      then
        do
          setFillStyle context "white"
          fillRect context rectangle
          setFillStyle context "black"
      else
        fillRect context rectangle

drawLine :: GameModel -> Number -> Number -> Number -> Number -> Effect Unit
drawLine game x0 y0 x1 y1 = do
  let dx = abs (x1 - x0)
  let dy = abs (y1 - y0)
  let sx = if x0 < x1 then 1.0 else -1.0
  let sy = if y0 < y1 then 1.0 else -1.0
  let err = dx - dy
  drawLineCore game x0 y0 x1 y1 dx dy sx sy err

drawLineCore :: GameModel -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
drawLineCore game x0 y0 x1 y1 dx dy sx sy err = do
  invertPixel game x0 y0
  if ((x0 == x1) && (y0 == y1))
    then
      pure unit
    else
      let
        e2 = 2.0 * err
        x0err = if e2 > -dy
          then
            {x0: x0+sx, err: err-dy}
          else
            {x0: x0, err:err}
        y0err = (if e2 < dx
          then
            {y0: y0+sy, err: x0err.err+dx}
          else
            {y0: y0, err: x0err.err})
      in
        drawLineCore game x0err.x0 y0err.y0 x1 y1 dx dy sx sy y0err.err

display :: GameModel -> Effect Unit
display game | game.paintScreen = do
  if game.refreshScreen
    then case game.currentState of
      Menu -> menu game
      Aim -> level game
      _ -> pure unit
    else pure unit
  case game.currentState of
    Menu -> do
      let xCoordinate = 72.0
      clearScreen game
      let yCoordinate = getYCoordinateFromMenuOption game.menuOption
      fillText game.canvasContext "<" xCoordinate yCoordinate
    Aim -> do
      clearScreen game
      let rectangle = {x: game.currentX - 1.0, y: game.currentY - 1.0, width: 3.0, height: 3.0}
      fillRect game.canvasContext rectangle
      let endX = round (game.currentX + aimLength * sin(game.aimAngle))
      let endY = round (game.currentY - aimLength * cos(game.aimAngle))
      drawLine game game.currentX game.currentY endX endY
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