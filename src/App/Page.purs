module App.Page where

import App.Render
import Data.List
import Data.Maybe
import Data.Tuple
import Formula
import Parser
import Prelude
import Proof
import Verification

import Control.MonadPlus (empty)
import Data.Array (intersperse)
import Data.Array as Array
import Data.Traversable (sequence, traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { input :: String, rule :: Maybe Rule, proof :: Proof }
data Action = UpdateInput String | UpdateRule String | AppendProof

material :: Maybe Proof
material = readProof "|-(~A|B|-(A|-(~A|-(~B|-A,~A),~(~B),B),(B|-),~A|B,B),A->B),(A->B|-~A|B),(~A|B)<->(A->B)"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const { input: "", rule: Nothing, proof: Proof FTrue Nil }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

a :: Maybe (Capture -> Maybe (Tuple (List Int) Conclusion))
a = getProofCapture <$> material

b :: String -> Maybe (List Capture)
b input = readParser parseCaptures input

c :: String -> Maybe (List (Tuple (List Int) Conclusion))
c input = join $ (=<<) (map (sequence)) $ map sequence $ map (map (flap a)) $ b input

f :: String -> Maybe (List (Conclusion))
f input = map (map snd) (c input)

h :: Maybe (List (Conclusion)) -> List (List (Conclusion))
h Nothing = Nil
h (Just x) = singleton x

g :: forall w i. String -> Maybe (HH.HTML w i)
g input = HH.div_ <<< toUnfoldable <<< ((=<<) renderConclusion) <$> f input

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.div_
    -- [ renderMaybe (renderFlatProof (Just Reit : Just Ass : Nil) <<< flattenProof <$> material)
    [ renderFlatProof Nil $ flattenProof st.proof
    , HH.br_
    -- , renderMaybe
    --     (g st.input)
    -- , (HH.div_ <<< ((<>) [ HH.text "⊢" ]) <<< intersperse (HH.text ", ") <<< toUnfoldable)
    --     ( case st.rule of
    --         Just Ass -> pure $ HH.text "Assumption"
    --         Just Reit -> pure $ HH.text "Reiteration"
    --         Just (Inf _ inf) -> HH.text <<< show <$> (inf =<< (toUnfoldable <$> (h $ f st.input)))
    --         _ -> pure $ HH.text ""
    --     )
    , HH.br_
    , HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Type in formula..."
        , HP.value st.input
        , HE.onValueInput UpdateInput
        ]
    -- , HH.select
    --     [ HE.onValueInput UpdateRule ]
    --     ( [ HH.option_ [ HH.text "" ]
    --       ]
    --         <> (map (HH.option_ <<< Array.singleton <<< HH.text <<< fst) ruleTable)
    --     )
    , HH.button
        [ HE.onClick (const AppendProof) ]
        [ HH.text "Append to proof" ]
    ]

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction (UpdateInput s) = H.modify_
  ( \st ->
      let
        s' = maybe s show (readFormula s)
      in
        st { input = s' }
  )
handleAction (UpdateRule r) = H.modify_ (\st -> st { rule = readRule r })
handleAction AppendProof =
  H.modify_
    ( \st ->
        let
          f = readFormula st.input
          proof' = maybe st.proof (flip appendProof st.proof) f
        in
          st { input = "", proof = proof' }
    )
