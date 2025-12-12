module Main where

import Prelude

import BF.Compression (fflateGZipSync)
import BF.Const (defaultNPrograms, defaultProgramLength)
import BF.Data.ArrayView
  ( Float32Array
  , SharedArrayBuffer
  , Uint32Array
  , Uint8Array
  , fromSharedBuffer
  , index
  , slice
  , writeAt
  )
import BF.Data.Utils (randomSharedArrayBuffer, zerosSharedArrayBuffer)
import BF.Metrics
  ( Metrics
  , getNInteractions
  , metricsFromBuffers
  , newMetricsBuffers
  , readRecordLatest
  )
import BF.Permutation (newPermutationBuffer)
import BF.Signals (newMasterCommandBuffer, newWorkerStatusBuffer)
import BF.SimulationConfig (WorkerConfigMessage)
import Control.Monad.Rec.Class (Step(..), forever, tailRec, tailRecM)
import Control.Monad.Rec.Class (forever)
import Data.Array ((..))
import Data.DateTime.Instant (Instant, diff)
import Data.Foldable (for_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int (fromString, toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Symbol (class IsSymbol)
import Data.Time (Millisecond)
import Data.Time.Duration (class Duration, Milliseconds(..), Seconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, delay, error, forkAff, killFiber, launchAff_, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, StateId, UseEffect)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Log (_debug, getTimeElapsedString)
import Plot (createLineChart)
import Prim.Row as Row
import Record (get)
import Type.Proxy (Proxy(..))
import Utils (rangeLoop, toLocaleString)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Worker.Worker
  ( Worker
  , defaultWorkerOptions
  , new
  , onMessage
  , postMessage
  , terminate
  )

debug = _debug "main"

main :: Effect Unit
main = do
  HA.runHalogenAff do
    maybeAppDiv <- HA.selectElement (QuerySelector "#app")
    case maybeAppDiv of
      Nothing -> do
        liftEffect $ debug "App container not found"
        pure unit
      Just appDiv -> do
        _ <- runUI mainComponent unit appDiv
        pure unit

  log "Done"

type UseMetrics = Hooks.UseState ExperimentData <> Hooks.UseEffect <> Hooks.Pure

type ExperimentData = { nInteractions :: Int, rate :: Number }

useMetrics
  :: forall m
   . MonadAff m
  => Metrics
  -> Instant
  -> Hook m UseMetrics (Maybe ExperimentData)
useMetrics metrics startTime = Hooks.do
  experimentData /\ experimentDataID <- Hooks.useState { nInteractions: 0, rate: -1.0 }

  Hooks.useLifecycleEffect do
    forkId <- Hooks.fork do
      forever do
        currentNInteractions <- liftEffect $ getNInteractions metrics
        currentTime <- liftEffect now
        let
          (Seconds seconds) = diff currentTime startTime :: Seconds
          rate = (toNumber currentNInteractions) / seconds :: Number
        Hooks.modify_ experimentDataID $ _
          { nInteractions = currentNInteractions, rate = rate }
        liftAff $ delay $ Milliseconds 1000.0

    pure $ Just $ Hooks.kill forkId
  Hooks.pure $ Just experimentData

type ExperimentSettings = { nPrograms :: Int, nWorkers :: Int }

experimentComponent
  :: forall q i o m. MonadAff m => ExperimentSettings -> H.Component q i o m
experimentComponent settings = Hooks.component \_ _ -> Hooks.do
  objectsContainer /\ objectsContainerId <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    objectsValue <- liftEffect do
      let
        { nWorkers, nPrograms } = settings
        programLength = defaultProgramLength

      beforeInitializeSoup <- now
      soup <- randomSharedArrayBuffer $ nPrograms * programLength
      elapsedString <- getTimeElapsedString beforeInitializeSoup
      debug $ elapsedString <> " Initialized Soup"

      metricsBuffers <- newMetricsBuffers
      permutationBuffer <- newPermutationBuffer nPrograms
      masterCommandBuffer <- newMasterCommandBuffer
      workerStatusBuffer <- newWorkerStatusBuffer nPrograms
      let
        metrics = metricsFromBuffers metricsBuffers

      for_ (0 .. (nWorkers - 1)) \workerIndex -> do
        debug $ "Starting worker " <> show workerIndex
        let isMaster = workerIndex == 0
        worker <- new "./worker.js" defaultWorkerOptions
          { name = "worker-" <> show workerIndex }
        worker # onMessage \ev -> debug ev
        worker # postMessage
          ( { soup
            , nPrograms
            , programLength
            , metricsBuffers
            , permutationBuffer
            , masterCommandBuffer
            , workerStatusBuffer
            , isMaster
            , workerIndex
            , nWorkers
            } :: WorkerConfigMessage
          )

      startTime <- now
      pure { metrics, startTime, soup }
    Hooks.modify_ objectsContainerId $ const $ Just objectsValue
    pure Nothing

  Hooks.pure do
    HH.div_
      [ HH.a [ HP.href "/", HP.target "_blank" ] [ HH.text "Start a new experiment" ]
      , HH.p_ [ HH.text $ show settings ]
      , case objectsContainer of
          Nothing -> HH.text "Loading"
          Just { metrics, startTime, soup } -> do
            HH.div_
              [ HH.slot_ _exp unit (_experimentComponent metrics startTime) unit
              , HH.slot_ _plot unit (plotComponent metrics soup) unit
              ]
      ]

_experimentComponent
  :: forall q i o m. MonadAff m => Metrics -> Instant -> H.Component q i o m
_experimentComponent metrics startTime = Hooks.component \_ _ -> Hooks.do
  experimentData <- useMetrics metrics startTime
  Hooks.pure do
    case experimentData of
      Nothing -> HH.p_ [ HH.text $ "Something went wrong" ]
      Just { nInteractions, rate } -> HH.p_
        [ HH.text $ "Interactions: " <> toLocaleString nInteractions
        , HH.br_
        , HH.text $ "Rate: " <> toLocaleString rate <> " interactions per second"
        ]

type Slots =
  ( exp :: forall q o. H.Slot q Void o
  , plot :: forall q o. H.Slot q Void o
  , mainExp :: forall q o. H.Slot q Void o
  )

_plot = Proxy :: Proxy "plot"
_exp = Proxy :: Proxy "exp"
_mainExp = Proxy :: Proxy "mainExp"

mainComponent
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
mainComponent = Hooks.component \_ _ -> Hooks.do
  startDemo /\ startDemoId <- Hooks.useState false

  settings /\ settingsId <- Hooks.useState
    { nPrograms: defaultNPrograms, nWorkers: 5 }

  Hooks.pure do
    HH.div_
      [ HH.p_
          [ HH.text "This is an implementation in the browser of "
          , HH.a
              [ HP.href "https://arxiv.org/pdf/2406.19108"
              , HP.target "_blank"
              ]
              [ HH.text
                  "Computational Life: How Well-formed, Self-replicating Programs Emerge from Simple Interaction"
              ]
          , HH.text ". The source code is available on "
          , HH.a
              [ HP.href "https://github.com/ricky0123/complexity"
              , HP.target "_blank"
              ]
              [ HH.text "GitHub" ]
          , HH.text "."
          ]
      , if (not startDemo) then HH.div_
          [ HH.div_
              [ HH.label_
                  [ HH.text "Number of programs" ]
              , HH.input
                  [ HP.type_ InputNumber
                  , HP.value $ show settings.nPrograms
                  , HP.min 10.0
                  , HP.max 1000000.0
                  , HE.onValueInput \raw ->
                      Hooks.modify_ settingsId \old -> old
                        { nPrograms = fromMaybe old.nPrograms (Int.fromString raw) }
                  ]
              ]
          , HH.div_
              [ HH.label_
                  [ HH.text "Number of workers" ]
              , HH.input
                  [ HP.type_ InputNumber
                  , HP.value $ show settings.nWorkers
                  , HP.min 2.0
                  , HP.max 500.0
                  , HE.onValueInput \raw ->
                      Hooks.modify_ settingsId \old -> old
                        { nWorkers = fromMaybe old.nWorkers (Int.fromString raw) }
                  ]
              ]
          , HH.button
              [ HE.onClick \_ -> Hooks.modify_ startDemoId not ]
              [ HH.text "Start experiment" ]
          ]
        else HH.slot_ _mainExp unit (experimentComponent settings) unit
      ]

plotComponent
  :: forall q i o m. MonadAff m => Metrics -> SharedArrayBuffer -> H.Component q i o m
plotComponent metrics soup = Hooks.component \_ _ -> Hooks.do
  let
    chartLabel = H.RefLabel "chart"
    chartId = "chart"

  Hooks.useLifecycleEffect do
    refResult <- Hooks.getRef chartLabel
    case refResult of
      Nothing -> do
        liftEffect $ debug "Chart div doesn't exist"
        pure Nothing
      Just _ -> do
        updateFn <- liftEffect $ createLineChart
          { title: "Simulation", elemId: chartId }

        liftAff $ delay $ Milliseconds 30_000.0
        compressionInteractionsArrayBuf <- liftEffect $ zerosSharedArrayBuffer
          (4 * 4_000_000)
        compressionArrayBuf <- liftEffect $ zerosSharedArrayBuffer (4 * 4_000_000)
        let
          compressionInteractions =
            fromSharedBuffer compressionInteractionsArrayBuf :: Uint32Array
          compression = fromSharedBuffer compressionArrayBuf :: Uint32Array

        forkId <- Hooks.fork do
          let
            go ctx@{ noOpsEpoch, compressionEpoch } = do
              liftEffect $ debug "Getting plot data"

              currentInteractions <- liftEffect $ getNInteractions metrics
              liftEffect $ writeAt compressionInteractions compressionEpoch
                currentInteractions

              timeIt "Got compression after" do
                let uint8Soup = fromSharedBuffer soup :: Uint8Array
                complexity <- liftEffect $ fflateGZipSync uint8Soup
                liftEffect $ writeAt compression compressionEpoch complexity
                liftEffect $ debug $ "Complexity = " <> show complexity

              newNoOpsEpoch <- liftEffect $ readRecordLatest
                metrics.noOpMetricRecord
                noOpsEpoch

              let
                noOpsInteractions = slice metrics.interactionMetricRecord 0
                  newNoOpsEpoch
                noOps = slice metrics.noOpMetricRecord 0 newNoOpsEpoch
                currentCompressionInteractions = slice compressionInteractions 0
                  compressionEpoch
                currentCompression = slice compression 0 compressionEpoch

              liftEffect $ debug noOps
              liftEffect $ debug currentCompression

              liftEffect $ updateFn
                { noOps: { interactions: noOpsInteractions, noOps }
                , compression:
                    { interactions: currentCompressionInteractions
                    , compression: currentCompression
                    }
                }

              liftAff $ delay $ Milliseconds 120_000.0
              pure $ Loop ctx
                { noOpsEpoch = newNoOpsEpoch, compressionEpoch = compressionEpoch + 1 }
          tailRecM go { noOpsEpoch: 0, compressionEpoch: 0 }

        pure Nothing

  Hooks.pure do
    HH.div_
      [ HH.div
          [ HP.id chartId, HP.ref chartLabel, HP.style "width: 1000px; height: 600px;" ]
          []
      ]

timeIt :: forall m a. MonadEffect m => String -> m a -> m a
timeIt logStr comp = do
  startTime <- liftEffect now
  res <- comp
  timeStr <- liftEffect $ getTimeElapsedString startTime
  liftEffect $ debug $ logStr <> " " <> timeStr
  pure res
