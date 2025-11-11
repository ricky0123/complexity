module BF.Simulation where

import Prelude

import BF.Const (defaultIterations, defaultMaxSteps)
import BF.Const (nonNoOps)
import BF.Data.ArrayView
  ( BigInt64Array
  , Int32Array
  , SharedArrayBuffer
  , Uint32Array
  , Uint8Array
  , atomicsLoad
  , fromSharedBuffer
  , fromSharedBufferSubset
  )
import BF.Data.Program (BF(..), runBF)
import BF.Data.Utils (foldMapUint8Array)
import BF.Metrics (Metrics, incrementInteractionCount, writeMetrics)
import BF.Permutation (fastShuffle)
import BF.Signals
  ( signalReady
  , signalStartWorkers
  , signalStopWorkers
  , waitForStartSignal
  , waitForWorkersReady
  , workerStopRequested
  )
import BF.SimulationConfig (MasterConfig, NonMasterConfig)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Rec.Class (class MonadRec, Step(..), forever, tailRecM, whileJust)
import Data.Array (elem)
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Int (quot, rem, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Time.Duration (class Duration, Seconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Random (randomInt)
import Foreign (Foreign, ForeignError, readInt, unsafeFromForeign)
import Foreign.Index ((!))
import Log (_debug)
import Utils (rangeLoop)
import Web.Worker.GlobalScope (onMessage)
import Web.Worker.MessageEvent (data_)

debug = _debug "Simulation"

type SimContext_ =
  ( nPrograms :: Int
  , soup :: SharedArrayBuffer
  , programLength :: Int
  , metrics :: Metrics
  )

type NonMasterSimContext = Record SimContext_
type MasterSimContext = { epoch :: Int, nIterations :: Int | SimContext_ }

getInteractionBF :: Int -> Int -> Int -> SharedArrayBuffer -> BF
getInteractionBF first second programLength sharedBuffer =
  let
    firstTape = fromSharedBufferSubset sharedBuffer (first * programLength)
      programLength
    secondTape = fromSharedBufferSubset sharedBuffer (second * programLength)
      programLength
    cut = programLength
    size = 2 * programLength
  in
    BF { firstTape, secondTape, cut, size }

startNonMasterWorker
  :: NonMasterConfig
  -> Effect Unit
startNonMasterWorker
  { nPrograms
  , programLength
  , soup
  , metrics
  , permutations
  , masterCommand
  , workerStatus
  , workerIndex
  , nWorkers
  } =
  tailRecM
    go
    { soup
    , nPrograms
    , programLength
    , metrics
    }
  where
  go :: NonMasterSimContext -> Effect (Step NonMasterSimContext _)
  go ctx = do
    let
      workerName = "worker " <> show workerIndex
    debug $ "top of worker " <> workerName
    let
      Tuple permStart permEnd =
        getWorkerPermutationEndpoints nWorkers nPrograms workerIndex
    -- signal idle and ready
    signalReady workerStatus workerIndex

    -- wait for start signal
    waitForStartSignal masterCommand
    debug $ workerName <> " got start signal"

    -- iterate
    _ <- whileJust do
      Tuple i j <- samplePrograms permutations permStart permEnd
      let program = getInteractionBF i j programLength soup
      _ <- runBF defaultMaxSteps program
      incrementInteractionCount metrics
      stop <- workerStopRequested masterCommand
      if stop then do
        debug $ workerName <> " got stop signal"
        pure Nothing
      else pure $ Just unit

    pure $ Loop ctx

startMasterWorker
  :: MasterConfig
  -> Effect Unit
startMasterWorker
  { nPrograms
  , programLength
  , soup
  , metrics
  , permutations
  , masterCommand
  , workerStatus
  , workerIndex
  , nWorkers
  } =
  tailRecM
    go
    { nIterations: defaultIterations
    , soup
    , nPrograms
    , programLength
    , metrics
    , epoch: 0
    }
  where
  go :: MasterSimContext -> Effect (Step MasterSimContext _)
  go ctx@{ epoch, nIterations } = do
    debug $ "top of worker " <> show workerIndex
    let
      Tuple permStart permEnd =
        getWorkerPermutationEndpoints nWorkers nPrograms workerIndex
    -- wait for workers to be idle and ready
    waitForWorkersReady nWorkers workerStatus

    -- record current interactions/no-ops
    writeMetrics nPrograms programLength soup metrics epoch

    -- create new permutation
    fastShuffle permutations

    debug "Shuffled and going to signal start"

    -- tell workers to start
    signalStartWorkers masterCommand

    -- iterate on own segment
    start <- now
    _ <- rangeLoop nIterations \_ _ -> do
      Tuple i j <-
        samplePrograms permutations permStart permEnd :: Effect (Tuple Int Int)
      let program = getInteractionBF i j programLength soup
      _ <- runBF defaultMaxSteps program :: Effect BF
      pure unit

    signalStopWorkers masterCommand

    end <- now
    let
      (Seconds seconds) = diff end start :: Seconds
      rate = (toNumber nIterations) / seconds
      nextIterations = round $ rate * 20.0
    -- noOpProportion = toNumber nNoOps / toNumber (nPrograms * programLength)
    debug $ "'Local' rate " <> show (rate * 6.0)

    -- restart
    pure $ Loop ctx { nIterations = nextIterations, epoch = epoch + 1 }

getWorkerPermutationEndpoints :: Int -> Int -> Int -> Tuple Int Int
getWorkerPermutationEndpoints nWorkers nPrograms workerPlace =
  let
    quotient = quot nPrograms nWorkers
    remainder = rem nPrograms nWorkers
    start = workerPlace * quotient
    end =
      if (workerPlace == nWorkers - 1) then (workerPlace + 1) * quotient - 1 + remainder
      else (workerPlace + 1) * quotient - 1
  in
    Tuple start end

samplePrograms :: Uint32Array -> Int -> Int -> Effect (Tuple Int Int)
samplePrograms permutations start end = do
  first <- randomInt start (end - 1)
  x <- randomInt start (end - 1)
  let second = if (x >= first) then (x + 1) else x
  firstPermuted <- atomicsLoad permutations first
  secondPermuted <- atomicsLoad permutations second
  pure $ Tuple firstPermuted secondPermuted
