module App.Render where

import Prelude

import Data.Array (replicate)
import Data.List (List(..), foldl, length, toUnfoldable, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.Common (enumerate)
import Model.Proof (FlatConclusion(..), Proof, flattenProof)
import Model.Scope (Scope, Scoped)
import Model.Verification (Capture, Rule, verify)

type State = { rules :: List (Maybe (Tuple Rule (Array Capture))), proof :: Proof, error :: String }
data Action = UpdateInput String | UpdateRule Int String

renderProof :: forall w. List (Maybe (Tuple Rule (Array Capture))) -> Proof -> HH.HTML w Action
renderProof rules p = HH.table [ HP.style "display: inline-block;" ] <<< toUnfoldable <<< rfc 0 (enumerate rules) $ fp
  where
  fp = flattenProof p

  rfc :: forall w'. Int -> List (Tuple Int (Maybe (Tuple Rule (Array Capture)))) -> List (Scoped FlatConclusion) -> List (HH.HTML w' Action)
  rfc n ((Tuple i r) : rs) (c : cs) = HH.tr_ [ HH.td_ <<< pure <<< HH.text <<< show $ n, HH.td_ (renderFlatConclusion c), HH.td_ [ HH.text (maybe "" (show') r) ], HH.td_ <<< pure $ (HH.input [ HE.onValueInput (UpdateRule i) ]) ] : rfc (n + 1) rs cs
    where
    show' :: Tuple Rule (Array Capture) -> String
    -- show' (Tuple r' []) = show r' <> " " <> show'' (verify p c <$> r)
    show' r' = show (fst r') <> ", " <> joinWith ", " ((map show <<< snd) r') <> " " <> show'' (verify p n c r')

    show'' :: (Maybe Boolean) -> String
    -- The capture is inaccessible
    show'' (Nothing) = "~"
    show'' (Just false) = "✗"
    show'' (Just true) = "✓"

  rfc n Nil (c : cs) = HH.tr_ [ HH.td_ <<< pure <<< HH.text <<< show $ n, HH.td_ (renderFlatConclusion c), HH.td_ [ HH.text "X" ], HH.td_ <<< pure <<< HH.text $ "" ] : rfc (n + 1) Nil cs
  rfc _ _ Nil = Nil

renderPage :: forall cs m. State -> H.ComponentHTML Action cs m
renderPage st =
  HH.span_
    [ HH.textarea
        [ HP.placeholder "Type in proof..."
        , HE.onValueInput UpdateInput
        , HP.style "display: inline-block; width: 50%;"
        ]
    -- , HH.br_
    , renderProof st.rules st.proof
    ]

-- renderConclusion :: forall w i. Conclusion -> List (HH.HTML w i)
-- renderConclusion (SubFormula f) = HH.text (show f) : HH.br_ : Nil
-- renderConclusion (SubProof p) = renderProof p : HH.br_ : Nil

-- renderProof :: forall w i. Proof -> HH.HTML w i
-- renderProof (Proof ass concs) =
--   HH.div [ (HP.class_ (HC.ClassName "proof")) ]
--     [ HH.div [ HP.class_ (HC.ClassName "scope") ] (toUnfoldable ((HH.u_ [ HH.text (show' ass) ]) : HH.br_ : ((concatMap renderConclusion (reverse concs)))))
--     ]
--   where
--   -- NOTE: Nice edge case where the assumption is top, which is the same as having no assumptions
--   show' :: Formula -> String
--   show' FTrue = "__"
--   show' f = show f

renderScope :: Scope -> String
renderScope = (foldl (<>) "") <<< flip replicate "\x2005\x2005\x2005\x2005|" <<< length

renderFlatConclusion :: forall w i. Scoped FlatConclusion -> Array (HH.HTML w i)
renderFlatConclusion (Tuple scope (Assumption a)) = [ HH.text (renderScope scope), HH.a [ HP.class_ (HC.ClassName "assumption enscoped") ] [ HH.u_ <<< pure <<< HH.text $ (show a) ] ]
renderFlatConclusion (Tuple scope (Consequence a)) = [ HH.text (renderScope scope), HH.a [ HP.class_ (HC.ClassName "consequence enscoped") ] [ HH.text (show a) ] ]
