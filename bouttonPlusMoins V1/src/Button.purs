module Button  where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type  Model = {compteur :: Int} 

data Msg
  = 
  Augmente 
  | Diminue 
  


component :: forall m. H.Component HH.HTML (Const Void) Unit Void m 
component = H.mkComponent
  { initialState: const initialState
  , render : view
  , eval: H.mkEval $ H.defaultEval
    { handleAction = update }
  }
  where
  initialState :: Model
  initialState = {  compteur : 0}
    
update :: forall m. Msg -> H.HalogenM Model Msg () Void m Unit
update Augmente    =
  do
    state <-  H.get
    H.put  {compteur :  state.compteur + 1}  

update Diminue  =
  do
    state <-  H.get
    H.put  {compteur :  state.compteur - 1}

view :: forall m. Model -> H.ComponentHTML Msg () m
view model =
    HH.div_
      [ HH.h3_
          [ HH.text "Compteur version 1 " ]
      , HH.button
              [ HE.onClick $ Just <<< const Diminue  ]
              [ HH.text "Diminue" ]
          
      ,  HH.text $ show model.compteur 
      ,  HH.button
              [ HE.onClick $ Just <<< const Augmente ]
              [ HH.text "Augmente" ]
          
      ]
