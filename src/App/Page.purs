module App.Page where

import App.Render
import Data.List
import Data.Maybe
import Data.Tuple
import Formula
import Parser
import Prelude
import Proof
import Scope
import Verification

import Control.Apply (lift3)
import Data.Either (either)
import Data.String (joinWith)
import Data.Unfoldable (replicate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { input :: String, rules :: List (Maybe (Tuple Rule (Array Capture))), proof :: FlatProof, scope :: Scope, error :: String }
data Action = UpdateInput String | UpdateRule Int String

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const { input: "", rules: Just (Tuple Ass []) : Nil, proof: flattenProof $ Proof FTrue Nil, scope: 0 : Nil, error: "" }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

renderFlatProof :: forall w. List (Maybe (Tuple Rule (Array Capture))) -> FlatProof -> HH.HTML w Action
renderFlatProof rules fp = HH.table_ <<< toUnfoldable <<< rfc 0 (enumerate rules) $ fp
  where
  rfc :: forall w'. Int -> List (Tuple Int (Maybe (Tuple Rule (Array Capture)))) -> List (Scoped FlatConclusion) -> List (HH.HTML w' Action)
  rfc n ((Tuple i r) : rs) (c : cs) = HH.tr_ [ HH.td_ <<< pure <<< HH.text <<< show $ n, HH.td_ (renderFlatConclusion c), HH.td_ [ HH.text (maybe "" (show') r) ], HH.td_ <<< pure $ (HH.input [ HE.onValueInput (UpdateRule i) ]) ] : rfc (n + 1) rs cs
    where
    show' :: Tuple Rule (Array Capture) -> String
    show' (Tuple r' []) = show r' <> " " <> show'' (lift3 verify (unflattenProof fp) (Just c) r)
    show' (Tuple r' cs') = show r' <> ", " <> joinWith ", " (map show cs') <> " " <> show'' (lift3 verify (unflattenProof fp) (Just c) r)

    show'' :: Maybe (Maybe Boolean) -> String
    show'' Nothing = "Could not unflatten proof"
    -- show'' (Just (Nothing)) = "Rule incorrectly applied or out of scope"
    show'' (Just (Nothing)) = "✗"
    show'' (Just (Just false)) = "✗"
    show'' (Just (Just true)) = "✓"

  rfc n Nil (c : cs) = HH.tr_ [ HH.td_ <<< pure <<< HH.text <<< show $ n, HH.td_ (renderFlatConclusion c), HH.td_ [ HH.text "X" ], HH.td_ <<< pure <<< HH.text $ "" ] : rfc (n + 1) Nil cs
  rfc _ _ Nil = Nil

check :: Proof -> FlatProof -> List (Maybe (Tuple Rule (Array Capture))) -> List String
check p Nil _ = Nil
check p (fp : fps) Nil = "Unjustified" : check p fps Nil
check p (fp : fps) (Nothing : rs) = "Maybe" : check p fps rs
check p (fp : fps) (Just r : rs) =
  ( case verify p fp r of
      Nothing -> "Maybe"
      Just true -> "Good"
      Just false -> "Bad"
  ) : check p fps rs

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.div_
    [ HH.textarea
        [ HP.placeholder "Type in proof..."
        , HE.onValueInput UpdateInput
        ]
    , HH.br_
    , renderFlatProof st.rules st.proof
    ]

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction (UpdateRule l r) = H.modify_
  ( \st ->
      let
        r' = readRule r
        rules' = maybe st.rules identity ((\x -> updateAt l x st.rules) r')
      in
        st { rules = rules' }
  )
handleAction (UpdateInput s) =
  H.modify_
    ( \st ->
        let
          p = readProof s
          proof' = either (const st.proof) flattenProof p
          input' = either (const s) (const "") p
          rules' =
            let
              rulesLen = length st.rules
              proofLen = length proof'
            in
              case compare rulesLen proofLen of
                LT -> st.rules <> replicate (proofLen - rulesLen) Nothing
                EQ -> st.rules
                GT -> take proofLen st.rules
        in
          st { input = input', proof = proof', error = either identity (const "") p, rules = rules' }
    )
