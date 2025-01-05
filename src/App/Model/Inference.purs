module Inference where

import Data.Maybe
import Formula
import Prelude
import Proof

import Data.List
import Control.Plus (empty)

-- import Data.Set (Set)

type Inference = (Array Conclusion -> Maybe Formula)

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
findContradiction = contradiction <<< catMaybes <<< map getFormula

notIntro :: Inference
notIntro [ SubProof (Proof a as) ]
  | findContradiction (SubFormula a : as) = pure (FNot a)
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

orIntroL :: Inference
orIntroL [ SubFormula f ] = pure (FOr f FMeta)
orIntroL _ = empty

orIntroR :: Inference
orIntroR [ SubFormula f ] = pure (FOr FMeta f)
orIntroR _ = empty

orElim :: Inference
orElim [ SubFormula (FOr a b), SubProof (Proof c (SubFormula d : _)), SubProof (Proof e (SubFormula f : _)) ]
  | a == c, b == e, d == f = pure d
orElim _ = empty

impIntro :: Inference
impIntro [ SubProof (Proof a (SubFormula b : _)) ] = pure (FImp a b)
impIntro _ = empty

impElim :: Inference
impElim [ SubFormula (FImp a b), SubFormula c ]
  | a == c = pure b
impElim _ = empty

iffIntro :: Inference
iffIntro [ SubProof (Proof a (SubFormula b : _)), SubProof (Proof c (SubFormula d : _)) ]
  | a == d, b == c = pure (FIff a b)
iffIntro _ = empty

iffElimL :: Inference
iffElimL [ SubFormula (FIff a b), SubFormula c ]
  | a == c = pure b
iffElimL _ = empty

iffElimR :: Inference
iffElimR [ SubFormula (FIff a b), SubFormula c ]
  | b == c = pure a
iffElimR _ = empty
