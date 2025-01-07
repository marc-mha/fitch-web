module Test.Main where

import Prelude

import Data.Maybe
import Effect (Effect)
import Effect.Class.Console (log)
import Parser
import Proof

material :: Maybe Proof
material = readLineProof "|-(~A|B|-(A|-(~A|-(~B|-A,~A),~(~B),B),(B|-),~A|B,B),A->B),(~A|B)->(A->B)"

main :: Effect Unit
main = do
  log $ maybe "Cannot parse!" showProof material
