module Model.Scope where

import Prelude

import Data.List (List(..), reverse, (:))
import Data.Tuple (Tuple)

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
