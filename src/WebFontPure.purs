module WebFontPure where

import Prelude

import Effect (Effect)

type WebFontConfigGoogle = 
  {
    families :: Array String
  }
type WebFontConfig = 
  {
    google :: WebFontConfigGoogle,
    active :: Effect Unit
  }
foreign import load :: WebFontConfig -> Effect Unit