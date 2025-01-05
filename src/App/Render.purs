module App.Render where

import Data.List (List(..), concatMap, reverse, toUnfoldable, (:))
import Data.Maybe (Maybe(..))
import Prelude
import Proof (Conclusion(..), Proof(..))

import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP

renderConclusion :: forall w i. Conclusion -> List (HH.HTML w i)
renderConclusion (SubFormula f) = HH.text (show f) : HH.br_ : Nil
renderConclusion (SubProof p) = renderProof p : Nil

renderProof :: forall w i. Proof -> HH.HTML w i
renderProof (Proof ass concs) =
  HH.div [ (HP.class_ (HC.ClassName "proof")) ]
    [ HH.div [ HP.class_ (HC.ClassName "scope") ] (toUnfoldable ((HH.u_ [ HH.text (show ass) ]) : HH.br_ : ((concatMap renderConclusion (reverse concs)))))
    ]

renderMaybe :: forall w i. Maybe (HH.HTML w i) -> HH.HTML w i
renderMaybe (Just h) = h
renderMaybe Nothing = HH.p_ [ HH.text "Could not parse!" ]
