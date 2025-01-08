module Model.Inference where

import Prelude

import Model.Proof (Conclusion(..), Proof, assumption, conclusions, extractFormula)
import Model.Formula (Formula(..))

import Data.List (List(..), catMaybes, elem, intersect, (:))
import Control.Plus (empty)

-- import Data.Set (Set)

type InferenceM m = (Array Conclusion -> m Formula)

type Inference = InferenceM List

-- NOTE: To be used for switching between logic systems with different inference rules,
-- such as intuitionistic logic, as well as different preferences in the same system.
-- type Logic = Set Inference

contradiction :: List Formula -> Boolean
contradiction cs = FFalse `elem` cs || contradiction' cs
  where
  contradiction' Nil = false
  contradiction' (a : as) = go a as || contradiction' as
    where
    go (FNot c) bs = c `elem` bs || (FNot (FNot c)) `elem` bs
    go b bs = (FNot b) `elem` bs

findContradiction :: List Conclusion -> Boolean
findContradiction = contradiction <<< catMaybes <<< map extractFormula

formulas :: Proof -> List Formula
formulas = catMaybes <<< map extractFormula <<< conclusions

notIntro :: Inference
notIntro [ SubProof p ]
  | findContradiction (conclusions p) = pure (FNot (assumption p))
notIntro _ = empty

notElim :: Inference
notElim [ SubFormula (FNot (FNot a)) ] = pure a
notElim _ = empty

andIntro :: Inference
andIntro [ SubFormula f, SubFormula g ] = pure (FAnd f g)
andIntro _ = empty

andElimL :: Inference
andElimL [ SubFormula (FAnd a _) ] = pure a
andElimL _ = empty

andElimR :: Inference
andElimR [ SubFormula (FAnd _ b) ] = pure b
andElimR _ = empty

andElim :: Inference
andElim prems = andElimL prems <> andElimR prems

orIntroL :: Inference
orIntroL [ SubFormula f ] = pure (FOr f FMeta)
orIntroL _ = empty

orIntroR :: Inference
orIntroR [ SubFormula f ] = pure (FOr FMeta f)
orIntroR _ = empty

orIntro :: Inference
orIntro prems = orIntroL prems <> orIntroR prems

orElim :: Inference
orElim [ SubFormula (FOr a b), SubProof p, SubProof q ]
  | a == assumption p, b == assumption q = intersect (formulas p) (formulas q)
orElim _ = empty

impIntro :: Inference
impIntro [ SubProof p ] = (FImp (assumption p) <$> formulas p)
impIntro _ = empty

impElim :: Inference
impElim [ SubFormula (FImp a b), SubFormula c ]
  | a == c = pure b
impElim _ = empty

iffIntro :: Inference
iffIntro [ SubProof p, SubProof q ]
  | assumption p `elem` formulas q, assumption q `elem` formulas p = pure (FIff (assumption p) (assumption q))
iffIntro _ = empty

iffElimL :: Inference
iffElimL [ SubFormula (FIff a b), SubFormula c ]
  | a == c = pure b
iffElimL _ = empty

iffElimR :: Inference
iffElimR [ SubFormula (FIff a b), SubFormula c ]
  | b == c = pure a
iffElimR _ = empty

iffElim :: Inference
iffElim prems = iffElimL prems <> iffElimR prems

canReplace :: Formula -> Formula -> Boolean
canReplace _ FMeta = true
canReplace FMeta _ = false
canReplace FTrue FTrue = true
canReplace FFalse FFalse = true
canReplace (FAtom s) (FAtom t) = s == t
canReplace (FNot a) (FNot b) = canReplace a b
canReplace (FAnd a b) (FAnd c d) = canReplace a c && canReplace b d
canReplace (FOr a b) (FOr c d) = canReplace a c && canReplace b d
canReplace (FImp a b) (FImp c d) = canReplace a c && canReplace b d
canReplace (FIff a b) (FIff c d) = canReplace a c && canReplace b d
canReplace _ _ = false
