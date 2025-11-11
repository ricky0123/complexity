module BF.Metrics where

import Prelude

import BF.Const (nonNoOps)
import BF.Data.ArrayView
  ( BigInt64Array
  , Float32Array
  , SharedArrayBuffer
  , Uint32Array
  , Uint8Array
  , atomicsAdd
  , atomicsExchange
  , atomicsLoad
  , fromSharedBuffer
  , fromSharedBufferSubset
  , length
  , slice
  )
import BF.Data.Utils (foldMapUint8Array, zerosSharedArrayBuffer)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (elem)
import Data.Int (toNumber)
import Data.Monoid.Additive (Additive(..))
import Data.Semiring (one)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import JS.BigInt (BigInt)
import Log (_debug)

debug = _debug "Metrics"

type Metrics =
  { interactionCounter :: Uint32Array
  , interactionMetricRecord :: Uint32Array
  , noOpMetricRecord :: Uint32Array
  }

type MetricsBuffers =
  { interactionCounterBuffer :: SharedArrayBuffer
  , interactionMetricRecordBuffer :: SharedArrayBuffer
  , noOpMetricRecordBuffer :: SharedArrayBuffer
  }

incrementInteractionCount :: Metrics -> Effect Unit
incrementInteractionCount metrics = atomicsAdd metrics.interactionCounter 0 one

newMetricsBuffers
  :: Effect
       MetricsBuffers
newMetricsBuffers = do
  -- big int array of length 1
  interactionCounterBuffer <- zerosSharedArrayBuffer (8 * 1)

  -- Int32Array length 4,000,000. In future should be a resizable abstraction, or written to indexeddb
  interactionMetricRecordBuffer <- zerosSharedArrayBuffer (4 * 4_000_000)

  -- float32 array length 4,000,000
  noOpMetricRecordBuffer <- zerosSharedArrayBuffer (4 * 4_000_000)
  pure
    { interactionCounterBuffer
    , interactionMetricRecordBuffer
    , noOpMetricRecordBuffer
    }

metricsFromBuffers
  :: MetricsBuffers
  -> Metrics
metricsFromBuffers
  { interactionCounterBuffer, interactionMetricRecordBuffer, noOpMetricRecordBuffer } =
  { interactionCounter: fromSharedBuffer interactionCounterBuffer
  , interactionMetricRecord: fromSharedBuffer interactionMetricRecordBuffer
  , noOpMetricRecord: fromSharedBuffer noOpMetricRecordBuffer
  }

getNInteractions :: Metrics -> Effect Int
getNInteractions { interactionCounter } = atomicsLoad interactionCounter 0

writeMetrics :: Int -> Int -> SharedArrayBuffer -> Metrics -> Int -> Effect Unit
writeMetrics
  nPrograms
  programLength
  soup
  metrics@
    { interactionCounter
    , interactionMetricRecord
    , noOpMetricRecord
    }
  epoch = do
  nInteractions <- getNInteractions metrics
  let
    nNoOps = countNoOps $ fromSharedBuffer soup
    noOpProportion = toNumber nNoOps / toNumber (nPrograms * programLength)
  _ <- atomicsExchange interactionMetricRecord epoch nInteractions
  _ <- atomicsExchange noOpMetricRecord epoch nNoOps
  pure unit

countNoOps :: Uint8Array -> Int
countNoOps arr = result
  where
  realOpIndicator :: Int -> Additive Int
  realOpIndicator i = if (i `elem` nonNoOps) then (Additive 0) else (Additive 1)

  Additive result = foldMapUint8Array realOpIndicator arr

readRecordLatest
  :: Uint32Array
  -> Int
  -> Effect Int
readRecordLatest metricArray start = do
  tailRecM go { i: start, max: recordLength }
  where
  recordLength = length metricArray
  go ctx@{ i, max }
    | i >= max - 1 = do
        debug "This happened"
        pure $ Done i
    | otherwise = do
        val <- atomicsLoad metricArray i
        if (val == 0) then do
          debug "What the fuck"
          pure $ Done i
        else pure $ Loop ctx { i = i + 1 }
