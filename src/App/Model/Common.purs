module Model.Common where

import Prelude
import Data.List (List(..), length, take, (:))
import Data.Tuple (Tuple(..))
import Control.Plus (class Plus, empty)
import Data.Unfoldable (replicate)

enumerate :: forall a. List a -> List (Tuple Int a)
enumerate = enumerate' 0
  where
  enumerate' _ Nil = Nil
  enumerate' n (x : xs) = (Tuple n x) : enumerate' (n + 1) xs

resize :: forall f a. Plus f => List (f a) -> Int -> List (f a)
resize l n =
  let
    rulesLen = length l
  in
    case compare rulesLen n of
      LT -> l <> replicate (n - rulesLen) empty
      EQ -> l
      GT -> take n l
