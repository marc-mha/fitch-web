module App.Page where

import Prelude

import Parser (readProof, readRule)
import App.Render (Action(..), State, renderPage)
import Model.Formula (Formula(..))
import Model.Proof (Proof(..), flattenProof)
import Model.Common (resize)

import Data.List (List(..), length, updateAt, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Halogen as H

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const { rules: Nothing : Nil, proof: Proof FTrue Nil, error: "" }
    , render: renderPage
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction (UpdateRule l r) = H.modify_
  ( \st ->
      let
        r' = readRule r
        rules' = maybe st.rules identity ((\x -> updateAt l x st.rules) r')
      in
        st { rules = rules' }
  )
handleAction (UpdateInput s) = H.modify_
  ( \st ->
      let
        p = readProof s
        proof' = either (const st.proof) identity p
        rules' = resize st.rules (length (flattenProof proof'))
      in
        st { proof = proof', error = either identity (const "") p, rules = rules' }
  )
