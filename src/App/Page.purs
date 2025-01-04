module App.Page where

import Data.Maybe
import Formula
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parser (readFormula)

type State = String
data Action = Update String

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const ""
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

renderFormula :: Maybe Formula -> String
renderFormula (Just f) = show f
renderFormula (Nothing) = "Could not parse!"

render :: forall cs m. State -> H.ComponentHTML Action cs m
render input =
  HH.div_
    [ HH.p_
        [ HH.text (renderFormula $ readFormula input) ]
    , HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Type in a formula..."
        , HE.onValueInput Update
        ]
    ]

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction (Update s) = H.modify_ (\_ -> s)
