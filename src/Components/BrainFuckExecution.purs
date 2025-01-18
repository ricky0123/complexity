module Components.BrainFuckExecution where

import Prelude

import BrainFuck.Program
import Data.Array
import Data.Char
import Data.Maybe
import Prelude

import Data.String as S
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Type.Proxy (Proxy(..))
import Utils.HTML (_classes)

type BFInput = { tape :: BrainFuckTape }

type BrainFuckComponentState = { executionContext :: BrainFuckExecutionContext }

data BrainFuckComponentAction
  = Initialize
  | HandleKey H.SubscriptionId KE.KeyboardEvent
  | StepForward

initialExecutionContext :: BrainFuckExecutionContext
initialExecutionContext =
  { instructionPointer: 0
  , readHead: 0
  , writeHead: 0
  , nStepsTaken: 0
  , tape: []
  , maxSteps: 1000
  , done: false
  }

brainfuck :: forall query output m. MonadEffect m => H.Component query BFInput output m
brainfuck =
  H.mkComponent
    { initialState: \input -> { executionContext: initialExecutionContext { tape = input.tape } }
    , render: bfComponentRender
    , eval: H.mkEval $ H.defaultEval
        { handleAction = bfComponentHandleAction
        , initialize = Just Initialize
        }
    }

bfComponentRender :: forall cs m. BrainFuckComponentState -> H.ComponentHTML BrainFuckComponentAction cs m
bfComponentRender state =
  HH.div_
    [ HH.h1
        [ _classes "mb-4" ]
        [ HH.text "Step through a Brainfuck Program" ]
    , HH.div
        [ _classes "d-flex flex-row gap-3" ]
        [ HH.div
            []
            [ HH.div
                [ _classes "card p-3"
                , HP.style "width: 250px;"
                ]
                [ HH.div
                    []
                    [ (HH.text $ "Steps taken: " <> show state.executionContext.nStepsTaken)
                    , HH.br_
                    , (HH.text $ "Instruction pointer: " <> show state.executionContext.instructionPointer)
                    , HH.br_
                    , (HH.text $ "Read head: " <> show state.executionContext.readHead)
                    , HH.br_
                    , (HH.text $ "Write head: " <> show state.executionContext.writeHead)
                    , HH.br_
                    , (HH.text $ "Done: " <> show state.executionContext.done)
                    , HH.br_
                    ]
                , HH.button
                    [ _classes "btn btn-outline-primary btn-sm mt-3"
                    , HE.onClick \_ -> StepForward
                    ]
                    [ HH.text "Forward" ]
                ]
            ]
        , HH.div
            [ _classes "row px-2" ]
            ( mapWithIndex
                ( \i -> \c ->
                    let
                      isCurrentInstruction = i == state.executionContext.instructionPointer
                      isCurrentReadHead = i == state.executionContext.readHead
                      isCurrentWriteHead = i == state.executionContext.writeHead
                    in
                      HH.div
                        [ HP.classes
                            [ HH.ClassName $
                                "mb-4 col-1 d-flex justify-content-center align-items-center"

                            ]
                        ]
                        ( [ HH.div
                              [ _classes $ "instruction-card"
                                  <> (if (isCurrentInstruction) then " text-bg-info" else "")
                                  <> (if (isCurrentReadHead || isCurrentWriteHead) then " position-relative" else "")
                              , HP.style "height: 24px; width: 40px;"
                              ]
                              ( [ HH.div
                                    [ _classes "h-100 w-100 d-flex justify-content-center align-items-center" ]
                                    [ showCodePoint c ]
                                ]
                                  <>
                                    ( if (not isCurrentReadHead) then ([] :: forall w i. Array (HH.HTML w i))
                                      else
                                        ( [ HH.div
                                              [ _classes "position-absolute read-head" ]
                                              [ HH.text "R" ]
                                          ] :: forall w i. Array (HH.HTML w i)
                                        )
                                    )
                                  <>
                                    if (not isCurrentWriteHead) then ([] :: forall w i. Array (HH.HTML w i))
                                    else
                                      ( [ HH.div
                                            [ _classes "position-absolute write-head" ]
                                            [ HH.text "W" ]
                                        ] :: forall w i. Array (HH.HTML w i)
                                      )
                              )
                          ]
                        )
                )
                state.executionContext.tape
            )
        ]
    ]

bfComponentHandleAction :: forall cs o m. MonadEffect m => BrainFuckComponentAction → H.HalogenM BrainFuckComponentState BrainFuckComponentAction cs o m Unit
bfComponentHandleAction = case _ of
  StepForward -> H.modify_ \st ->
    let
      nextCtx = fromMaybe st.executionContext do
        when st.executionContext.done Nothing
        executeBrainFuckStep st.executionContext
    in
      st { executionContext = nextCtx }
  Initialize -> do
    document <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  HandleKey _ ev
    | KE.key ev == "ArrowRight" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        bfComponentHandleAction StepForward

    | otherwise ->
        pure unit

showCodePoint i =
  let
    _default = HH.text $ "\\" <> show i

  in
    fromMaybe _default do
      when (i < firstPrintableAscii) Nothing
      when (i > lastPrintableAscii) Nothing
      char <- fromCharCode i
      let
        string = CodeUnits.fromCharArray [ char ]
      pure $ HH.text string
