module Scope where

import Data.List
import Data.Tuple
import Prelude

type Scope = List Int

type Scoped a = Tuple Scope a

isScope :: Scope -> Scope -> Boolean
isScope s t = s == t

-- Test whether left scope is within right scope
inScope :: Scope -> Scope -> Boolean
inScope s t = inScope' (reverse s) (reverse t)
  where
  inScope' :: Scope -> Scope -> Boolean
  inScope' _ Nil = true
  inScope' Nil _ = false
  inScope' (a : as) (b : bs)
    | a == b = inScope' as bs
    | otherwise = false

enumerate :: forall a. List a -> List (Tuple Int a)
enumerate = enumerate' 0
  where
  enumerate' _ Nil = Nil
  enumerate' n (x : xs) = (Tuple n x) : enumerate' (n + 1) xs

