module Components.Simulation where

import BrainFuck.Messages
import BrainFuck.Program
import Data.Array
import Data.Char
import Data.Maybe
import Prelude
import Utils.Log

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.Newtype (unwrap)
import Data.String as S
import Data.String.CodeUnits as CodeUnits
import Data.Time.Duration (Milliseconds(..), Seconds(..), fromDuration)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_, launchAff, runAff, forkAff, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Foreign (Foreign, ForeignError, readArray, readInt)
import Foreign.Index ((!))
import Graph.Graph (addDataPoint, addNewRecord, embedChart, VegaView)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Options.Applicative.Internal (P)
import Type.Proxy (Proxy(..))
import Utils.HTML (_classes)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.Worker.MessageEvent (data_)
import Web.Worker.Worker (defaultWorkerOptions, new, postMessage, onMessage, Worker, terminate)

type Input = { populationSize :: Int, programLength :: Int }

type State =
  { populationSize :: Int
  , programLength :: Int
  , nInteractions :: Int
  , lastComputations :: Int
  , recordPrograms :: RecordPrograms
  , plotElem :: H.RefLabel
  , view :: Maybe VegaView
  , elapsed :: Seconds
  , startTime :: Maybe Instant
  , worker :: Maybe Worker
  }

initialState :: State
initialState =
  { populationSize: -1
  , programLength: -1
  , nInteractions: 0
  , lastComputations: 0
  , recordPrograms: []
  , plotElem: H.RefLabel "plot"
  , view: Nothing
  , elapsed: Seconds 0.0
  , startTime: Nothing
  , worker: Nothing
  }

data Action = Initialize | Update StatusUpdate | Tick | Finalize

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
component = H.mkComponent
  { eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , finalize = Just Finalize
      , handleAction = handleAction
      }
  , render
  , initialState: \input -> initialState
      { populationSize = input.populationSize
      , programLength = input.programLength
      }
  }

handleAction :: forall cs output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action cs output m Unit
handleAction = case _ of
  Initialize -> do
    { populationSize, programLength } <- H.get
    { emitter, worker } <- H.liftEffect $ startWorker populationSize programLength
    H.modify_ \st -> st { worker = Just worker }
    _ <- H.subscribe emitter
    _ <- H.subscribe =<< timer Tick
    { plotElem } <- H.get
    mbElem <- H.getHTMLElementRef plotElem
    for_ mbElem \el -> do
      view <- H.liftAff $ embedChart $ HTMLElement.toElement el
      H.modify_ \st -> st { view = Just view }
    startTime <- H.liftEffect now
    H.modify_ \st -> st { startTime = Just startTime }

  Finalize -> do
    H.liftEffect $ log "Terminating worker"
    state <- H.get
    for_ state.worker \worker -> do
      H.liftEffect $ terminate worker

  Tick -> do
    state <- H.get
    for_ state.startTime \startTime -> do
      currentTime <- H.liftEffect now
      let (elapsed :: Seconds) = diff currentTime startTime
      H.modify_ \st -> st { elapsed = elapsed }

  Update update -> do
    H.modify_ \st -> st
      { nInteractions = update.nInteractions
      , lastComputations = update.lastComputations
      , recordPrograms = update.recordPrograms
      }
    state <- H.get
    for_ state.view \v -> do
      let
        x = toNumber update.nInteractions
        y = toNumber update.lastComputations
      H.liftEffect $ addDataPoint v x y
      for_ state.recordPrograms \record -> do
        H.liftEffect $ addNewRecord v (toNumber record.nInteractions)

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ forkAff $ forever do
    delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

render :: forall cs m. MonadEffect m => State -> H.ComponentHTML Action cs m
render st = HH.div_
  [ HH.h1
      [ _classes "mb-4" ]
      [ HH.text "Simulation Progress" ]
  , HH.div
      [ _classes "d-flex mb-4" ]
      [ HH.div
          [ _classes "card p-3" ]
          [ HH.text "Number of interactions: "
          , HH.text $ show st.nInteractions
          , HH.br_
          , HH.text "Average number of computations: "
          , HH.text $ show st.lastComputations
          , HH.br_
          , HH.text $ "Time elapsed: " <> (show $ Int.round $ unwrap st.elapsed) <> " seconds"
          , HH.br_
          , HH.text $ "Interactions per second: " <> (show $ Int.round (toNumber st.nInteractions / unwrap st.elapsed))
          ]
      ]
  , HH.div [ _classes "mb-4 d-flex justify-content-center", HP.ref st.plotElem ] []
  , HH.div
      [ _classes "d-flex" ]
      [ HH.div
          [ _classes "card p-3" ]
          [ HH.h5_ [ HH.text "Record programs" ]
          , HH.div [ _classes "list-group" ] $ map renderRecordProgram st.recordPrograms
          ]
      ]
  ]

renderRecordProgram :: forall w i. RecordProgram -> HH.HTML w i
renderRecordProgram record =
  let
    programDisplayString = fromMaybe "???" $ unParseBrainFuckProgram record.program
    programStr = (S.joinWith " ") $ map (\x -> "\\" <> show x) record.program
    dest = "#execution?program=" <> programStr
  in
    HH.a
      [ _classes "list-group-item list-group-item-action overflow-auto"
      , HP.href dest
      , HP.target "_blank"
      , HP.style "max-height: 5em;"
      ]
      [ HH.text $ show record.nInteractions <> " interactions, " <> show record.nComputations <> " computations"
      , HH.div
          [ _classes "px-4 py-2 rounded bg-light-subtle"
          , HP.style "font-family: monospace;"
          ]
          [ HH.text programDisplayString ]
      ]

readRecordProgram :: Foreign -> Except (NonEmptyList ForeignError) RecordProgram
readRecordProgram value = do
  programForeign <- value ! "program" >>= readArray
  program <- traverse readInt programForeign
  nComputations <- value ! "nComputations" >>= readInt
  nInteractions <- value ! "nInteractions" >>= readInt
  pure { program, nComputations, nInteractions }

readStatusUpdateMessage :: Foreign -> Except (NonEmptyList ForeignError) StatusUpdate
readStatusUpdateMessage value = do
  nInteractions <- value ! "nInteractions" >>= readInt
  lastComputations <- value ! "lastComputations" >>= readInt
  recordProgramsForeign <- value ! "recordPrograms" >>= readArray
  recordPrograms <- traverse readRecordProgram recordProgramsForeign
  pure { nInteractions, lastComputations, recordPrograms }

startWorker :: Int -> Int -> Effect { emitter :: HS.Emitter Action, worker :: Worker }
startWorker populationSize programLength = do
  worker <- new "./worker.js" defaultWorkerOptions
  updateSignal <- HS.create
  worker
    # onMessage \ev -> do
        let
          data' = data_ ev
          msg = runExcept $ readStatusUpdateMessage data'
        case msg of
          Left _ -> do
            log "Received unknown message"
            debug data'
          Right update ->
            HS.notify updateSignal.listener $ Update update
  worker # postMessage
    { populationSize
    , programLength
    }
  pure { emitter: updateSignal.emitter, worker: worker }
