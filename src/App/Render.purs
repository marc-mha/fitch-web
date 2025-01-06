module App.Render where

import Data.Foldable hiding (length)
import Data.List hiding (range)
import Data.Tuple
import Formula
import Prelude
import Proof
import Scope

import Data.Array (intersperse, range, replicate)
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Verification (Rule)

renderConclusion :: forall w i. Conclusion -> List (HH.HTML w i)
renderConclusion (SubFormula f) = HH.text (show f) : HH.br_ : Nil
renderConclusion (SubProof p) = renderProof p : HH.br_ : Nil

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

renderFitch :: forall w i. Proof -> Array (Maybe Rule) -> HH.HTML w i
renderFitch p rules =
  let
    numbers = range 0 (length $ flattenProof p)
    numberColumn = HH.div [ HP.class_ (HC.ClassName "numbers") ] $ intersperse (HH.br_) $ map (HH.a_ <<< pure <<< HH.text <<< show) numbers
    rulesColumn = HH.div [ HP.class_ (HC.ClassName "rules") ] $ intersperse (HH.br_) $ map (HH.a_ <<< pure <<< HH.text <<< (maybe "" show)) rules
  in
    HH.table_ [ HH.tr_ (map (HH.td_ <<< pure) [ numberColumn, renderProof p, rulesColumn ]) ]

padScope :: Scope -> String
padScope = (foldl (<>) "") <<< flip replicate "\x2005\x2005\x2005\x2005|" <<< length

renderFlatConclusion :: forall w i. Scoped FlatConclusion -> Array (HH.HTML w i)
renderFlatConclusion (Tuple scope (Assumption a)) = [ HH.text (padScope scope), HH.a [ HP.class_ (HC.ClassName "assumption enscoped") ] [ HH.u_ <<< pure <<< HH.text $ (show a) ] ]
renderFlatConclusion (Tuple scope (Consequence a)) = [ HH.text (padScope scope), HH.a [ HP.class_ (HC.ClassName "consequence enscoped") ] [ HH.text (show a) ] ]

zipMaybe :: forall a b. List (Maybe b) -> List a -> List (Tuple (Maybe b) a)
zipMaybe _ Nil = Nil
zipMaybe Nil (a : as) = (Tuple Nothing a) : zipMaybe Nil as
zipMaybe (b : bs) (a : as) = (Tuple b a) : zipMaybe bs as

renderFlatProof :: forall w i. List (Maybe Rule) -> FlatProof -> HH.HTML w i
renderFlatProof rules = HH.table_ <<< toUnfoldable <<< rfc 0 rules
  where
  rfc :: forall w' i'. Int -> List (Maybe Rule) -> List (Scoped FlatConclusion) -> List (HH.HTML w' i')
  rfc n (r : rs) (c : cs) = HH.tr_ [ HH.td_ <<< pure <<< HH.text <<< show $ n, HH.td_ (renderFlatConclusion c), HH.td_ <<< pure <<< HH.text $ maybe "" show r ] : rfc (n + 1) rs cs
  rfc n Nil (c : cs) = HH.tr_ [ HH.td_ <<< pure <<< HH.text <<< show $ n, HH.td_ (renderFlatConclusion c), HH.td_ <<< pure <<< HH.text $ "" ] : rfc (n + 1) Nil cs
  rfc _ _ Nil = Nil

renderMaybe :: forall w i. Maybe (HH.HTML w i) -> HH.HTML w i
renderMaybe (Just h) = h
renderMaybe Nothing = HH.p_ [ HH.text "Could not parse!" ]

renderList :: forall w i. List (HH.HTML w i) -> HH.HTML w i
renderList l = HH.div_ $ toUnfoldable l
