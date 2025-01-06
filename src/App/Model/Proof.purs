module Proof where

import Data.List
import Data.Maybe
import Data.Tuple
import Formula
import Scope
import Prelude

import Control.Apply (lift2)
import Data.Array (replicate)

data Proof = Proof Formula (List Conclusion)

instance Show Proof where
  show (Proof ass concs) = show ass <> " |- " <> show concs

data Conclusion = SubFormula Formula | SubProof Proof

instance Show Conclusion where
  show (SubFormula f) = show f
  show (SubProof p) = show p

data FlatConclusion = Assumption Formula | Consequence Formula

instance Show FlatConclusion where
  show (Assumption f) = "__ " <> show f
  show (Consequence f) = show f

type FlatProof = List (Scoped FlatConclusion)

assumption :: Proof -> Formula
assumption (Proof a _) = a

-- NOTE: Use snoc as assumptions are usually the first intended conclusion
conclusions :: Proof -> List Conclusion
conclusions (Proof a cs) = snoc cs (SubFormula a)

flattenConclusion' :: Scope -> Int -> Conclusion -> List (Scoped FlatConclusion)
flattenConclusion' s _ (SubFormula f) = pure (Tuple s (Consequence f))
flattenConclusion' s n (SubProof p) = flattenProof' (n : s) p

flattenProof' :: Scope -> Proof -> List (Scoped FlatConclusion)
flattenProof' s (Proof ass concs) = (Tuple s (Assumption ass)) : (enumerate (reverse concs) >>= uncurry (flattenConclusion' s))

flattenProof :: Proof -> FlatProof
flattenProof = flattenProof' (singleton 0)

-- FIX: Add proper way to enforce only jumping one scope when making a subproof
unflattenConclusions :: Scope -> List (Scoped FlatConclusion) -> Maybe (List (Conclusion))
unflattenConclusions _ Nil = Just Nil
unflattenConclusions scope ((Tuple fscope (Consequence f)) : sfs)
  | fscope `isScope` scope = (:) (SubFormula f) <$> unflattenConclusions fscope sfs
  | otherwise = Nothing
unflattenConclusions scope ((Tuple fscope (Assumption f)) : sfs)
  | fscope `inScope` scope, not (fscope `isScope` scope) =
      let
        { init: subconcs, rest: sfs' } = span ((flip inScope fscope) <<< fst) sfs
      in
        lift2 (:) (SubProof <$> unflattenProof (Tuple fscope (Assumption f) : subconcs)) (unflattenConclusions scope sfs')
  | otherwise = Nothing

unflattenProof :: List (Scoped FlatConclusion) -> Maybe Proof
unflattenProof ((Tuple fscope (Assumption f)) : sfs) = Proof f <$> (reverse <$> (unflattenConclusions fscope sfs))
unflattenProof _ = Nothing

unlines' :: List String -> String
unlines' Nil = ""
-- unlines' (s : Nil) = s
unlines' (s : ss) = "\n" <> s <> unlines' ss

showScope :: Int -> String
showScope = (foldl (<>) "") <<< flip replicate "|   "

showSub' :: Int -> Conclusion -> String
showSub' n (SubFormula f) = showScope n <> show f
showSub' n (SubProof p) = showProof' (n + 1) p

showProof' :: Int -> Proof -> String
showProof' n (Proof ass concs) = showScope (n - 1) <> "|__ " <> show' ass <> (unlines' <<< map (showSub' n) <<< reverse) concs
  where
  show' FTrue = ""
  show' x = show x

showProof :: Proof -> String
showProof = showProof' 1

extractFormula :: Conclusion -> Maybe Formula
extractFormula (SubFormula f) = Just f
extractFormula _ = Nothing

extractFlatFormula :: FlatConclusion -> Formula
extractFlatFormula (Assumption f) = f
extractFlatFormula (Consequence f) = f
