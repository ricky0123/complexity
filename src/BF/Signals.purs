module BF.Signals where

import Prelude

import BF.Data.ArrayView
  ( Int32Array
  , SharedArrayBuffer
  , Uint32Array
  , atomicsExchange
  , atomicsLoad
  , atomicsNotify
  , fromSharedBuffer
  )
import BF.Data.Utils (waitForDifferentValue, zerosSharedArrayBuffer)
import Effect (Effect)
import Utils (rangeLoop)

newMasterCommandBuffer :: Effect SharedArrayBuffer
newMasterCommandBuffer = zerosSharedArrayBuffer (1 * 4)

getMasterCommandArray :: SharedArrayBuffer -> Int32Array
getMasterCommandArray = fromSharedBuffer

newWorkerStatusBuffer :: Int -> Effect SharedArrayBuffer
newWorkerStatusBuffer nPrograms = zerosSharedArrayBuffer $ (nPrograms - 1) * 4

getWorkerStatusArray :: SharedArrayBuffer -> Int32Array
getWorkerStatusArray = fromSharedBuffer

waitForWorkersReady :: Int -> Int32Array -> Effect Unit
waitForWorkersReady nWorkers workerStatus =
  unit <$ rangeLoop nWorkers \i _ -> do
    waitForDifferentValue workerStatus i 1

signalStartWorkers :: Int32Array -> Effect Unit
signalStartWorkers masterCommand = do
  _ <- atomicsExchange masterCommand 0 1
  atomicsNotify masterCommand 0

signalStopWorkers :: Int32Array -> Effect Unit
signalStopWorkers masterCommand = do
  _ <- atomicsExchange masterCommand 0 0
  atomicsNotify masterCommand 0

signalReady :: Int32Array -> Int -> Effect Unit
signalReady workerStatus workerIndex = do
  _ <- atomicsExchange workerStatus workerIndex 0
  atomicsNotify workerStatus workerIndex

waitForStartSignal :: Int32Array -> Effect Unit
waitForStartSignal masterCommand = waitForDifferentValue masterCommand 0 0

workerStopRequested :: Int32Array -> Effect Boolean
workerStopRequested masterCommand = do
  value <- atomicsLoad masterCommand 0
  pure $ value == 0
