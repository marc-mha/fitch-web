module App.Render where

import Formula
import Prelude

import Data.List (List(..), concatMap, reverse, toUnfoldable, (:))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Proof (Conclusion(..), Proof(..))

renderConclusion :: forall w i. Conclusion -> List (HH.HTML w i)
renderConclusion (SubFormula f) = HH.text (show f) : HH.br_ : Nil
renderConclusion (SubProof p) = renderProof p : Nil

renderProof :: forall w i. Proof -> HH.HTML w i
renderProof (Proof ass concs) =
  HH.div [ (HP.class_ (HC.ClassName "proof")) ]
    [ HH.div [ HP.class_ (HC.ClassName "scope") ] (toUnfoldable ((HH.u_ [ HH.text (show' ass) ]) : HH.br_ : ((concatMap renderConclusion (reverse concs)))))
    ]
  where
  -- NOTE: Nice edge case where the assumption is top, which is the same as having no assumptions
  show' :: Formula -> String
  show' FTrue = "__"
  show' f = show f

renderMaybe :: forall w i. Maybe (HH.HTML w i) -> HH.HTML w i
renderMaybe (Just h) = h
renderMaybe Nothing = HH.p_ [ HH.text "Could not parse!" ]

renderList :: forall w i. List (HH.HTML w i) -> HH.HTML w i
renderList l = HH.div_ $ toUnfoldable l
