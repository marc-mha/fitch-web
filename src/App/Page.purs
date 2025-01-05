module App.Page where

import Data.List
import Data.Maybe
import Formula
import Parser
import Prelude
import Proof
import App.Render

import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String
data Action = Update String

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const ""
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render input =
  HH.div_
    [ renderMaybe
        (renderProof <<< Proof FTrue <$> (readParser parseConclusions input))
    , HH.br_
    , HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Type in a proof..."
        , HE.onValueInput Update
        ]
    ]

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction (Update s) = H.modify_ (const s)
