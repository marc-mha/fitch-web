module Test.Main where

import Prelude

import Data.Maybe
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Test"
