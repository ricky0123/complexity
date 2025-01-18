module Main where

import BrainFuck.Program
import Components.BrainFuckExecution
import Data.Array
import Data.Char
import Data.Maybe
import Prelude

import BrainFuck.Examples (selfReplicatingTape)
import Components.Simulation as Simulation
import Components.SimulationConfig as SimulationConfig
import Data.Either (hush)
import Data.Foldable (oneOf)
import Data.Int as Int
import Data.String as S
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matches, matchesWith, setHash, getHash, match)
import Routing.Match (Match, bool, end, int, list, lit, nonempty, num, param, params, str)
import Type.Proxy (Proxy(..))
import Utils.HTML (_classes)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Components.ExecutionForm as ExecutionForm

data Route' = Home | SimulationRoute String String | SimulationConfigRoute | ExecutionRoute String

derive instance eqRoute' :: Eq Route'

home :: Match Route'
home = Home <$ end

simulationConfig :: Match Route'
simulationConfig = SimulationConfigRoute <$ (lit "simulation" <* lit "config") <* end

simulationExecution :: Match Route'
simulationExecution = SimulationRoute <$> (lit "simulation" *> param "population_size") <*> param "program_length" <* end

execution :: Match Route'
execution = ExecutionRoute <$> ((lit "execution") *> (param "program")) <* end

route'' :: Match Route'
route'' = oneOf
  [ home
  , simulationConfig
  , simulationExecution
  , execution
  ]

route' :: Match (Maybe Route')
route' = oneOf
  [ Just <$> route''
  , pure Nothing
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI baseComponent {} body

data Action = Initialize | RouteChanged (Maybe Route') | SetHash String

type Slots =
  ( brainfuck :: forall query. H.Slot query Void Int
  , simulation :: forall query. H.Slot query Void Int
  , simulationConfig :: forall query. H.Slot query Void Int
  , executionForm :: forall query. H.Slot query Void Int
  )

_brainfuck = Proxy :: Proxy "brainfuck"
_simulation = Proxy :: Proxy "simulation"
_simulationConfig = Proxy :: Proxy "simulationConfig"
_executionForm = Proxy :: Proxy "executionForm"

type BaseComponentState =
  { route :: Maybe Route'
  }

baseComponent :: forall q i o m. MonadEffect m => MonadAff m => H.Component q i o m
baseComponent =
  H.mkComponent
    { initialState: \_ -> { route: Just Home }
    , render: baseRender
    , eval: H.mkEval H.defaultEval { handleAction = baseHandleAction, initialize = Just Initialize }
    }

baseHandleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM BaseComponentState Action cs o m Unit
baseHandleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< H.liftEffect monitorRoute
    currentHash <- H.liftEffect getHash
    let
      maybeInitialRoute = hush $ match route' currentHash
      initialRoute = fromMaybe (Just Home) maybeInitialRoute
    H.modify_ \st -> st { route = initialRoute }
    pure unit
  RouteChanged newRoute -> do
    H.liftEffect $ log "Route change"
    H.modify_ \st -> st { route = newRoute }
  SetHash hash -> do
    H.liftEffect $ setHash hash
    pure unit

monitorRoute :: Effect (HS.Emitter Action)
monitorRoute = do
  { emitter, listener } <- HS.create
  void $ matches route' \old new ->
    when (old /= Just new) do
      HS.notify listener (RouteChanged new)
  pure emitter

baseRender :: forall m. MonadEffect m => MonadAff m => BaseComponentState -> H.ComponentHTML Action Slots m
baseRender state =
  let
    component = case state.route of
      Just Home -> HH.div_
        [ HH.h1_ [ HH.text "Welcome" ]
        , HH.p_
            [ HH.text "This is a web app that allows you to run an artificial life experiment in your browser. It is an implementation of the paper "
            , HH.a
                [ HP.href "https://arxiv.org/pdf/2406.19108"
                , HP.target "_blank"
                ]
                [ HH.text "Computational Life: How Well-formed, Self-replicating Programs Emerge from Simple Interaction" ]
            , HH.text " by Blaise Agüera y Arcas et al. For more information, check out the longer explanation below, listen to "
            , HH.a
                [ HP.href "https://www.preposterousuniverse.com/podcast/2024/08/19/286-blaise-aguera-y-arcas-on-the-emergence-of-replication-and-computation/"
                , HP.target "_blank"
                ]
                [ HH.text "this interview" ]
            , HH.text " with one of the authors on the Mindscape podcast, check out this relevant "
            , HH.a
                [ HP.href "https://arxiv.org/pdf/2406.19108"
                , HP.target "_blank"
                ]
                [ HH.text "article on Science.org" ]
            , HH.text ", or read the paper itself."
            ]
        , HH.h2_ [ HH.text "If you know what to do" ]
        , HH.p_ [ HH.text "You can start a simulation or walk through a single \"BFF\" program." ]
        , HH.div
            [ _classes "row justify-content-center" ]
            [ HH.slot_ _simulationConfig 0 SimulationConfig.component unit ]
        , HH.div
            [ _classes "row mb-3 justify-content-center" ]
            [ HH.slot_ _executionForm 0 ExecutionForm.component unit ]
        , HH.h2_ [ HH.text "What is this?" ]
        , HH.p_
            [ HH.text "This is an \"artificial life\" experiment. The experiment works by initializing a population of random "
            , HH.a
                [ HP.href "https://en.wikipedia.org/wiki/Brainfuck"
                , HP.target "_blank"
                ]
                [ HH.text "Brainfuck" ]
            , HH.text " programs. Then, for each step of the experiment, two randomly-selected programs are concatenated and run as a single program. The program operates on itself - there is no separate data array as in a standard Brainfuck program. Then the program is split apart into two halves again. Thus, the two original programs have \"interacted\" to produce two new programs. This is done for many iterations. The authors of the paper write that over time the population of programs becomes more complex, and an ever-changing ecosystem of self-replicating programs emerges."
            ]
        , HH.p_
            [ HH.text "As the experiment runs in your browser, it will display a graph of the number of \"computations\" in programs recently executed. By \"computations\", we mean the steps of a Brainfuck program that are not no-ops. At the start of the experiment, the randomly-initialized programs will tend to have a lot of no-ops because the programs are initialized with random ASCII characters, out which only a handful are Brainfuck commands that change the state of the system in any way. Over time, if the population of programs becomes more complex, that should be reflected in the graph." ]
        , HH.h2_ [ HH.text "How do I use it?" ]
        , HH.p_ [ HH.text "To start an experiment, fill out the first form above. You can set the population size (i.e. number of Brainfuck programs that are initialized and then interact with each other) and the program length (number of instructions in each Brainfuck program)." ]
        , HH.p_
            [ HH.text "You can also interactively step through a Brainfuck program to see how the language works. As mentioned above, this is actually a modified version of Brainfuck in which the program reads and writes to its own list of instructions rather than separate read and write tapes. For instance, "
            , HH.a
                [ HP.href "/#execution/1 2 3 4 5 6 7 8 9 0"
                , HP.target "_blank"
                ]
                [ HH.text "this program" ]
            , HH.text " shows an instance of the first half of a program copying itself onto the second half (figure 4 from the original paper)."
            ]
        , HH.h2_ [ HH.text "Why is it interesting?" ]
        , HH.p_
            [ HH.text "It's suprising that, over time, the program interactions lead to increasing complexity. Biological systems are, like Brainfuck, "
            , HH.a
                [ HP.href "https://www.embopress.org/doi/full/10.15252/embr.201846628"
                , HP.target "_blank"
                ]
                [ HH.text "information-processing systems" ]
            , HH.text " to some extent. Therefore, it's possible that that this experiment says something about the origin of life."
            ]
        , HH.div
            [ _classes "row justify-content-center" ]
            [ HH.blockquote
                [ _classes "blockquote col-lg-6" ]
                [ HH.p_ [ HH.text "I believe that I have somewhere said (but cannot find the passage) that the principle of continuity renders it probable that hereafter life will be shown to be a part or consequence of some general law; but this is only conjecture and not science" ]
                , HH.footer
                    [ _classes "blockquote-footer" ]
                    [ HH.a
                        [ HP.href "https://www.darwinproject.ac.uk/letter/DCP-LETT-13747.xml"
                        , HP.target "_blank"
                        ]
                        [ HH.text "Charles Darwin" ]
                    ]
                ]
            ]
        , HH.h2_ [ HH.text "Who are you?" ]
        , HH.p_
            [ HH.text "I'm a "
            , HH.a
                [ HP.href "https://ricky0123.com"
                , HP.target "_blank"
                ]
                [ HH.text "software engineer" ]
            , HH.text " that heard "
            , HH.a
                [ HP.href "https://www.preposterousuniverse.com/podcast/2024/08/19/286-blaise-aguera-y-arcas-on-the-emergence-of-replication-and-computation/"
                , HP.target "_blank"
                ]
                [ HH.text "an interview" ]
            , HH.text " with one of the authors and decided to make a browser implementation of the experiment. I used it as an opportunity to play with "
            , HH.a
                [ HP.href "https://pursuit.purescript.org"
                , HP.target "_blank"
                ]
                [ HH.text "PureScript" ]
            , HH.text ", "
            , HH.a
                [ HP.href "https://github.com/purescript-halogen/purescript-halogen"
                , HP.target "_blank"
                ]
                [ HH.text "Halogen" ]
            , HH.text ", and the "
            , HH.a
                [ HP.href "https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers"
                , HP.target "_blank"
                ]
                [ HH.text "Web Workers API" ]
            , HH.text ". You can find the source code on "
            , HH.a
                [ HP.href "https://github.com/ricky0123/life"
                , HP.target "_blank"
                ]
                [ HH.text "GitHub" ]
            , HH.text "."
            ]
        ]
      Just (ExecutionRoute program) -> case parseBrainFuckProgram program of
        Just tape -> HH.div_ [ HH.slot_ _brainfuck 0 brainfuck { tape } ]
        Nothing -> HH.text "Invalid program"
      Just SimulationConfigRoute -> HH.div_ [ HH.slot_ _simulationConfig 0 SimulationConfig.component unit ]
      Just (SimulationRoute maybePopulationSize maybeProgramLength) ->
        fromMaybe (HH.text "Invalid simulation parameters") do
          populationSize <- Int.fromString maybePopulationSize
          programLength <- Int.fromString maybeProgramLength
          pure $ HH.slot_ _simulation 0 Simulation.component { populationSize, programLength }
      Nothing -> HH.text "Not Found"
  in
    HH.div
      [ _classes "container pt-4 d-flex flex-column justify-content-between full-page" ]
      [ component
      , HH.div
          [ _classes "d-flex flex-wrap justify-content-end align-items-center p-2 mt-2 border-top" ]
          [ HH.div
              [ _classes "me-4 fst-italic" ]
              [ HH.text "A project by "
              , HH.a
                  [ HP.href "https://ricky0123.com"
                  , HP.target "_blank"
                  ]
                  [ HH.text "Ricky Samore" ]
              ]
          , HH.div_
              [ HH.a
                  [ HP.href "https://github.com/ricky0123/life"
                  , HP.target "_blank"
                  ]
                  [ HH.i
                      [ _classes "bi bi-github h2" ]
                      []
                  ]
              ]
          ]
      ]
