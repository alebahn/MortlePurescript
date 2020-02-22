module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler, effectCanceler)
import Effect.Class.Console (log)

import Web.HTML (window) as HTML
import Web.HTML.Window (toEventTarget)
import Web.Event.EventTarget (eventListener, addEventListener, removeEventListener)
import Web.Event.Event (EventType(..))

import Graphics.Canvas (getCanvasElementById)

import WebFontPure (load)

import Game (start) as Game

waitForWindowLoad :: Aff Unit
waitForWindowLoad = do
  window <- liftEffect HTML.window
  let eventType = EventType "load"
  let windowEventTarget = toEventTarget window
  makeAff (\listener -> do
    newEventListener <- eventListener(\_ -> listener $ Right unit)
    addEventListener eventType newEventListener false windowEventTarget
    pure $ effectCanceler $ removeEventListener eventType newEventListener false windowEventTarget)

loadFont :: Aff Unit
loadFont = makeAff (\listener -> do
  load {google: {families: ["Press Start 2P"]}, active: listener $ Right unit}
  pure nonCanceler)

main :: Effect Unit
main = launchAff_ do
  waitForWindowLoad
  loadFont
  maybeCanvasElement <- liftEffect $ getCanvasElementById "gameCanvas"
  case maybeCanvasElement of
    Nothing -> do
      liftEffect $ log "Failed to Load game canvas"
    Just canvasElement -> do
      Game.start canvasElement
