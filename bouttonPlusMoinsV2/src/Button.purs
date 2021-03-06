module Button  where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String(trim)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Increment = Int 

data MsgErr = PasDeSaisie | SaisieIncorrecte

data  Models = Accueil   (Either MsgErr Increment)
              | Compteur {compteur :: Int} Increment 
              

type State = { stage :: Models }


data Msg
  = 
  ChoisitValeur String
  | PasseDeLaccueilAuCompteur  
  | Augmente 
  | Diminue    
  | Recommencer 



component :: forall m. H.Component HH.HTML (Const Void) Unit Void m 
component = H.mkComponent
  { initialState  : const initialState
  , render        : view
  , eval          : H.mkEval $ H.defaultEval
      { handleAction = update }
  }

initialState :: State
initialState = {  stage : Accueil (Left PasDeSaisie)}

view :: forall m. State -> H.ComponentHTML Msg () m
view {stage  :  Accueil debut} = 
  HH.div_[
    HH.h2_[ HH.text "Compteur version 2" ]
     ,case debut of
        Right entier    ->
          HH.div_ [
            renderInput "correct" "entrez la valeur de l'incrément"
            ,renderNextButton (Right PasseDeLaccueilAuCompteur  )    "Cliquez pour continuer"
          ]
        Left PasDeSaisie    ->
          HH.div_ [
            renderInput "incorrect" "entrez la valeur de l'incrément"
            ,renderNextButton (Left "Le champs de l'incrément est vide")             "Cliquez pour continuer"
          ]
            
        Left SaisieIncorrecte ->
            HH.div_ [
              renderInput "incorrect" "entrez la valeur de l'incrément"
              ,renderNextButton (Left "Entrez une valeure entière"  )  "Cliquez pour continuer"
            ]
      ]
    

view { stage : Compteur model  _} =
  HH.div_ [
    HH.div_ [
      HH.h3_ [
        HH.button
          [ HE.onClick $ Just <<< const Diminue ]
          [ HH.text "Diminue" ]
        , HH.text $ show model.compteur 
        , HH.button
            [ HE.onClick $ Just <<< const Augmente ]
            [ HH.text "Augmente" ]
      ]
      ,renderNextButton ( Right Recommencer ) "Recommencer"
      ]
    ]

renderNextButton :: forall m. Either String Msg -> String -> H.ComponentHTML Msg() m
renderNextButton action message =
    case action of
      Left messageErreur  ->
        HH.p_ [
          HH.button [ HP.disabled true ]   [ HH.text message  ]
        , HH.br_, HH.text(messageErreur)]
      Right action'       ->
        HH.p_ [
          HH.button [ HE.onClick <<< const $ Just action' ]   [ HH.text message ]]

renderInput :: forall m. String -> String -> HH.HTML m Msg
renderInput nom textHolder =
  HH.input
    [
      HP.type_ HP.InputText
      , HP.name nom
      , HE.onValueInput(\str  -> Just (ChoisitValeur str))
      , HP.required true
      , HP.placeholder textHolder
    ]


update :: forall m. Msg -> H.HalogenM State Msg () Void m Unit
update (ChoisitValeur increment) = 
  
      case fromString (trim increment)  of 
        Just entier ->  H.modify_ _ {stage = Accueil (Right entier)}
        Nothing ->
          if trim increment == ""
          then H.modify_ _  { stage = Accueil  (Left PasDeSaisie)}
          else H.modify_ _  { stage = Accueil  (Left SaisieIncorrecte)}

update PasseDeLaccueilAuCompteur = 
    H.modify_ (\ etat ->
      case etat of 
        {stage : Accueil increment} -> 
          case increment of 
            Right entier -> 
              { stage : Compteur {compteur : 0 } entier }
            _ -> initialState
        _ -> etat )-- Model = Compteur

update Augmente  = 
    H.modify_ (\ etat ->
      case etat of 
        {stage : Compteur state increment} -> 
          { stage : Compteur {compteur : state.compteur +  increment} increment }
        _ -> etat )-- Model = Accueil

update Diminue   = 
    H.modify_ (\ etat ->
      case etat of 
        {stage : Compteur state increment} -> 
          { stage : Compteur {compteur : state.compteur -  increment} increment }
        _ -> etat )-- Model = Accueil

update Recommencer =
  H.modify_ (\etat -> initialState)


