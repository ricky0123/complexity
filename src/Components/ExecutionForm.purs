module Components.ExecutionForm where

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
import BrainFuck.Program (parseBrainFuckProgram)
import BrainFuck.Examples (selfReplicatingTape)

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( program :: f String String String
  )

type FormData = { | Form F.FieldOutput }

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

initialValues = { program: selfReplicatingTape }

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
        { program: \x -> case parseBrainFuckProgram x of
            Just _ -> Right x
            Nothing -> Left "Invalid program"
        }

      handleSuccess :: FormData -> H.HalogenM _ _ _ _ m Unit
      handleSuccess x = do
        H.liftEffect $ debug x
        let
          newPath = "#execution?program=" <> x.program
        H.liftEffect $ setHash newPath

    F.handleSubmitValidate handleSuccess F.validate validation

  render :: FormContext -> H.ComponentHTML Action () m
  render { formActions, fields, actions } =
    HH.form
      [ _classes "col-lg-6 mb-3"
      , HE.onSubmit formActions.handleSubmit
      ]
      [ HH.fieldset
          [ _classes "border rounded p-3 w " ]
          [ HH.legend
              [ _classes "float-none w-auto p-2" ]
              [ HH.text "Step through a BFF program" ]
          , HH.div
              [ _classes "d-flex flex-column align-items-end" ]
              [ HH.div
                  [ _classes "w-100 mb-3" ]
                  [ HH.label
                      [ _classes "form-label" ]
                      [ HH.text "Program" ]
                  , HH.input
                      [ _classes "form-control"
                      , HP.type_ HP.InputText
                      , HE.onValueInput actions.program.handleChange
                      , HE.onBlur actions.program.handleBlur
                      , case fields.program.result of
                          Nothing -> HP.placeholder selfReplicatingTape
                          Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                          Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
                      ]
                  ]
              , HH.button
                  [ _classes "btn btn-outline-primary"
                  , HP.type_ HP.ButtonSubmit
                  ]
                  [ HH.text "Start Simulation" ]
              ]
          ]
      ]