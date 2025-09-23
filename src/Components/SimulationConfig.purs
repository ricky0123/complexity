module Components.SimulationConfig where

import Prelude
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils.Log
import Data.Int as Int
import Routing.Hash (matches, matchesWith, setHash)
import Utils.HTML (_classes)

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( populationSize :: f String String Int
  , programSize :: f String String Int
  )

type FormData = { | Form F.FieldOutput }

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

initialValues = { populationSize: "200000", programSize: "64" }

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = F.formless { liftAction: Eval } initialValues $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> do
      H.liftEffect $ debug context
      H.put context
    Eval action -> do
      H.liftEffect $ debug action
      F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      validation :: { | Form F.FieldValidation }
      validation =
        { populationSize: \x -> case Int.fromString x of
            Nothing -> Left "Could not parse integer"
            Just i -> Right i
        , programSize: \x -> case Int.fromString x of
            Nothing -> Left "Could not parse integer"
            Just i -> Right i
        }

      handleSuccess :: FormData -> H.HalogenM _ _ _ _ m Unit
      handleSuccess x = do
        H.liftEffect $ debug x
        let
          newPath = "simulation?population_size=" <> show x.populationSize <> "&program_length=" <> show x.programSize
        H.liftEffect $ setHash newPath

    F.handleSubmitValidate handleSuccess F.validate validation

  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions } =
    HH.form
      [ _classes "col-lg-6 mb-1"
      , HE.onSubmit formActions.handleSubmit
      ]
      [ HH.fieldset
          [ _classes "border rounded p-3" ]
          [ HH.legend
              [ _classes "float-none w-auto p-2" ]
              [ HH.text "Start a simulation" ]
          , HH.div
              [ _classes "d-flex flex-row" ]
              [ HH.div
                  [ _classes "me-3 w-25" ]
                  [ HH.label
                      [ _classes "form-label" ]
                      [ HH.text "Population Size" ]
                  , HH.input
                      [ _classes "form-control"
                      , HP.type_ HP.InputText
                      , HE.onValueInput actions.populationSize.handleChange
                      , HE.onBlur actions.populationSize.handleBlur
                      , case fields.populationSize.result of
                          Nothing -> HP.placeholder "200000"
                          Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                          Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                      ]
                  ]
              , HH.div
                  [ _classes "me-3 w-25" ]
                  [ HH.label
                      [ _classes "form-label" ]
                      [ HH.text "Program Length" ]
                  , HH.input
                      [ _classes "form-control"
                      , HP.type_ HP.InputText
                      , HE.onValueInput actions.programSize.handleChange
                      , HE.onBlur actions.programSize.handleBlur
                      , case fields.programSize.result of
                          Nothing -> HP.placeholder "64"
                          Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                          Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                      ]
                  ]
              , HH.div
                  [ _classes "w-50 d-flex justify-content-end align-items-end" ]
                  [ HH.button
                      [ _classes "btn btn-outline-primary"
                      , HP.type_ HP.ButtonSubmit
                      ]
                      [ HH.text "Start Simulation" ]
                  ]
              ]
          ]
      ]