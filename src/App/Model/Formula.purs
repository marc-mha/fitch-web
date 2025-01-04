module Formula where

import Prelude

data Formula
  = FTrue
  | FFalse
  | FMeta
  | FAtom String
  | FNot Formula
  | FAnd Formula Formula
  | FOr Formula Formula
  | FImp Formula Formula
  | FIff Formula Formula

showFormula :: Formula -> String
showFormula (FAtom s) = s
showFormula FTrue = "⊤"
showFormula FFalse = "⊥"
showFormula FMeta = "φ"
showFormula (FNot f) = "¬" <> showFormula f
showFormula (FAnd f g) = "(" <> showFormula f <> " ∧ " <> showFormula g <> ")"
showFormula (FOr f g) = "(" <> showFormula f <> " ∨ " <> showFormula g <> ")"
showFormula (FImp f g) = "(" <> showFormula f <> " → " <> showFormula g <> ")"
showFormula (FIff f g) = "(" <> showFormula f <> " ↔ " <> showFormula g <> ")"

instance Show Formula where
  show = showFormula

derive instance Eq Formula
