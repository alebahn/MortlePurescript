module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_, makeAff, effectCanceler)
import Effect.Class.Console (log)

import Web.HTML (window) as HTML
import Web.HTML.Window (toEventTarget)
import Web.Event.EventTarget (eventListener, addEventListener, removeEventListener)
import Web.Event.Event (EventType(..))

import Graphics.Canvas (getCanvasElementById, getContext2D, setFont, fillText)

waitForWindowLoad :: Aff Unit
waitForWindowLoad = do
  window <- liftEffect HTML.window
  let eventType = EventType "load"
  let windowEventTarget = toEventTarget window
  makeAff (\listener -> do
    newEventListener <- eventListener(\_ -> listener $ Right unit)
    addEventListener eventType newEventListener false windowEventTarget
    pure $ effectCanceler $ removeEventListener eventType newEventListener false windowEventTarget)

main :: Effect Unit
main = launchAff_ do
  waitForWindowLoad
  liftEffect do
    maybeCanvasElement <- getCanvasElementById "gameCanvas"
    case maybeCanvasElement of
      Nothing -> do
        log "Nothing"
      Just canvasElement -> do
        context <- getContext2D canvasElement
        setFont context "8px 'Press Start 2P'"
        fillText context "MORTLE" 24.0 12.0
        log "🍝"
