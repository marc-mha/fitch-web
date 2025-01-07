module Verification where

import Control.MonadPlus
import Data.List hiding (unzip)
import Data.Maybe
import Data.Tuple
import Formula
import Inference
import Prelude
import Proof
import Scope

import Data.Array (unzip)
import Data.Bifunctor (rmap)
import Data.Traversable (sequence, traverse)

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

  Tuple scope <$> (unflattenProof s)

getProofCapture :: Proof -> Capture -> Maybe (Scoped Conclusion)
getProofCapture p (Line n) = rmap SubFormula <$> getProofFormula p n
getProofCapture p (Lines n m) = rmap SubProof <$> getProofProof p n m

data Rule
  = Ass
  | Reit
  | Inf String Inference

instance Show Rule where
  show Ass = "A"
  show Reit = "R"
  show (Inf tag _) = tag

infer :: Scope -> Proof -> Array Capture -> Inference -> Maybe (List Formula)
infer scope p captures inf = do
  (Tuple cscopes premises) :: Tuple (Array Scope) (Array Conclusion) <- unzip <$> traverse (getProofCapture p) captures

  -- Check that all premises are in the same scope
  -- NOTE: Change to `inScope` to remove need for reiteration
  guard (all ((scope `isScope` _)) cscopes)

  -- Return the result of the inference
  pure (inf premises)

verify :: Proof -> Scoped FlatConclusion -> (Tuple Rule (Array Capture)) -> Maybe Boolean
verify p (Tuple scope f) (Tuple r cs) = do
  inferences :: List Formula <- case r of
    Ass -> case f of
      Assumption _ -> pure (singleton FMeta)
      Consequence _ -> empty
    Reit -> case cs of
      [ Line l ] -> do
        (Tuple cscope f') <- getProofFormula p l
        guard (scope `inScope` cscope && not (scope `isScope` cscope))
        pure (singleton f')
      _ -> empty
    Inf _ inf -> infer scope p cs inf
  pure <<< any (canReplace (extractFlatFormula f)) $ inferences
-- pure <<< any ((==) (extractFlatFormula f)) $ inferences
