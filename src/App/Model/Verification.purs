module Verification where

import Data.List
import Data.Maybe
import Formula
import Inference
import Prelude
import Proof
import Scope

import Control.MonadPlus (guard)
import Data.Bifunctor (rmap)
import Data.Tuple

data Capture = Line Int | Lines Int Int

instance Show Capture where
  show (Line n) = show n
  show (Lines n m) = show n <> "-" <> show m

getProofFormula :: Proof -> Int -> Maybe (Scoped Formula)
getProofFormula p n = rmap extractFlatFormula <$> index (flattenProof p) n

getProofProof :: Proof -> Int -> Int -> Maybe (Scoped Proof)
getProofProof p n m = do
  guard (m >= n)
  let
    fp = flattenProof p
    s = slice n (m + 1) fp
  guard (length s == m - n + 1)
  -- NOTE: Take the tail to get rid of the scope jump that is caused by starting a proof
  scope <- tail =<< fst <$> head s
  case s of
    --- Edge case of A |- A
    ass : Nil -> pure $ Tuple scope (Proof (extractFlatFormula $ snd ass) ((SubFormula $ extractFlatFormula $ snd ass) : Nil))
    _ -> Tuple scope <$> (unflattenProof s)

getProofCapture :: Proof -> Capture -> Maybe (Scoped Conclusion)
getProofCapture p (Line n) = rmap SubFormula <$> getProofFormula p n
getProofCapture p (Lines n m) = rmap SubProof <$> getProofProof p n m

data Rule
  = Ass
  | Reit
  | Inf Inference
