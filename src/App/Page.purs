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

import Data.Array as Array
import Data.Traversable (sequence, traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String
data Action = Update String

material :: Maybe Proof
material = readProof "|-(~A|B|-(A|-(~A|-(~B|-A,~A),~(~B),B),(B|-),~A|B,B),A->B),(~A|B)->(A->B)"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const ""
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

a :: Maybe (Capture -> Maybe (Tuple (List Int) Conclusion))
a = getProofCapture <$> material

b :: String -> Maybe (List Capture)
b input = readParser parseCaptures input

c :: String -> Maybe (List (Tuple (List Int) Conclusion))
c input = join $ (=<<) (map (sequence)) $ map sequence $ map (map (flap a)) (b input)

f :: String -> Maybe (List (Conclusion))
f input = map (map snd) (c input)

g input = HH.div_ <<< toUnfoldable <<< ((=<<) renderConclusion) <$> f input

render :: forall cs m. State -> H.ComponentHTML Action cs m
render input =
  HH.div_
    [ renderMaybe (renderProof <$> material)
    , HH.br_
    , renderMaybe
        -- (renderProof <<< Proof FTrue <$> (readParser parseConclusions input))
        -- (renderProof <<< Proof FTrue <$> f input)
        (g input)
    , HH.br_
    , HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Type in a proof..."
        , HE.onValueInput Update
        ]
    ]

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction (Update s) = H.modify_ (const s)
