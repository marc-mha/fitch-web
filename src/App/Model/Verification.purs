module Model.Verification where

import Prelude

import Model.Formula (Formula(..))
import Model.Inference (Inference, canReplace)
import Model.Proof (Conclusion(..), FlatConclusion(..), Proof, extractFlatFormula, flattenProof, unflattenProof)
import Model.Scope (Scope, Scoped, inScope, isScope)

import Control.MonadPlus (empty, guard)
import Data.List (List, all, any, head, index, length, singleton, slice, tail)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Array (unzip)
import Data.Bifunctor (rmap)
import Data.Traversable (traverse)

data Capture = Line Int | Lines Int Int

before :: Capture -> Int -> Boolean
before (Line m) l = m < l
before (Lines _ n) l = n < l

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

verify :: Proof -> Int -> Scoped FlatConclusion -> (Tuple Rule (Array Capture)) -> Maybe Boolean
verify p n (Tuple scope f) (Tuple r cs) = do
  guard (all (_ `before` n) cs)
  inferences :: List Formula <- case r of
    -- Assumptions can only be correctly applied to assumptions
    Ass -> case f of
      -- Anything can be an assumption, so return meta
      Assumption _ -> pure (singleton FMeta)
      -- If you try to apply assumption to a consequence, tut tut
      Consequence _ -> pure empty
    Reit -> case cs of
      [ Line l ] -> do
        (Tuple cscope f') <- getProofFormula p l
        -- This should not be a false as it might be allowed, the capture is
        -- just not acceptable
        guard (scope `inScope` cscope && not (scope `isScope` cscope))
        pure (singleton f')
      _ -> pure empty
    Inf _ inf -> infer scope p cs inf
  pure (any (canReplace (extractFlatFormula f)) inferences)
