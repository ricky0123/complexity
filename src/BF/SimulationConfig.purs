module BF.SimulationConfig where

import Prelude

import BF.Data.ArrayView (Int32Array, SharedArrayBuffer, Uint32Array, fromSharedBuffer)
import BF.Metrics (Metrics, MetricsBuffers, metricsFromBuffers)
import BF.Permutation (getPermutationArray)
import BF.Signals (getMasterCommandArray, getWorkerStatusArray)
import Control.Monad.Except (Except, ExceptT, runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Foreign (Foreign, ForeignError(..), fail, unsafeFromForeign)
import Foreign.Index (class Index, hasOwnProperty, hasProperty, (!))
import Web.Worker.MessageEvent (data_)
import Web.Worker.Types (MessageEvent)

type WorkerConfigMessage =
  { nPrograms :: Int
  , programLength :: Int
  , soup :: SharedArrayBuffer
  , metricsBuffers :: MetricsBuffers
  , permutationBuffer :: SharedArrayBuffer
  , masterCommandBuffer :: SharedArrayBuffer
  , workerStatusBuffer :: SharedArrayBuffer
  , workerIndex :: Int
  , nWorkers :: Int
  , isMaster :: Boolean
  }

type MasterConfig =
  { nPrograms :: Int
  , programLength :: Int
  , soup :: SharedArrayBuffer
  , metrics :: Metrics
  , permutations :: Uint32Array
  , masterCommand :: Int32Array
  , workerStatus :: Int32Array
  , workerIndex :: Int
  , nWorkers :: Int
  }

type NonMasterConfig =
  { nPrograms :: Int
  , programLength :: Int
  , soup :: SharedArrayBuffer
  , metrics :: Metrics
  , permutations :: Uint32Array
  , masterCommand :: Int32Array
  , workerStatus :: Int32Array
  , workerIndex :: Int
  , nWorkers :: Int
  }

data ConfigParseResult e
  = ParseError e
  | ParsedMaster MasterConfig
  | ParsedNonMaster NonMasterConfig

derive instance eqConfigParseResult :: Eq e => Eq (ConfigParseResult e)
derive instance genericConfigParseResult :: Generic (ConfigParseResult e) _

instance showConfigParseResult :: Show e => Show (ConfigParseResult e) where
  show c = genericShow c

readProp
  :: forall a m
   . Index String m
  => Monad m
  => Foreign
  -> String
  -> ExceptT (NonEmptyList ForeignError) m a
readProp obj key =
  if (hasProperty key obj) then unsafeFromForeign <$> (obj ! key)
  else fail $ ForeignError ("couldn't read prop " <> key)

parseWorkerConfigMessage :: Foreign -> ConfigParseResult _
parseWorkerConfigMessage msg =
  let
    parsed = runExcept do
      soup <- readProp msg "soup" :: Except _ SharedArrayBuffer
      nPrograms <- readProp msg "nPrograms" :: Except _ Int
      programLength <- readProp msg "programLength" :: Except _ Int
      metricsBuffers <-
        readProp msg "metricsBuffers" :: Except _ MetricsBuffers
      permutationBuffer <-
        readProp msg "permutationBuffer"
          :: Except _ SharedArrayBuffer
      masterCommandBuffer <-
        readProp msg "masterCommandBuffer"
          :: Except _ SharedArrayBuffer
      workerStatusBuffer <-
        readProp msg "workerStatusBuffer"
          :: Except _ SharedArrayBuffer
      workerIndex <- readProp msg "workerIndex" :: Except _ Int
      nWorkers <- readProp msg "nWorkers" :: Except _ Int
      isMaster <- readProp msg "isMaster" :: Except _ Boolean

      let
        metrics = metricsFromBuffers metricsBuffers
        permutations = fromSharedBuffer permutationBuffer
        masterCommand = getMasterCommandArray masterCommandBuffer
        workerStatus = getWorkerStatusArray workerStatusBuffer

      pure $
        if isMaster then ParsedMaster
          { soup
          , nPrograms
          , programLength
          , metrics
          , permutations
          , masterCommand
          , workerStatus
          , workerIndex
          , nWorkers
          }
        else ParsedNonMaster
          { soup
          , nPrograms
          , programLength
          , metrics
          , permutations
          , masterCommand
          , workerStatus
          , workerIndex
          , nWorkers
          }
  in
    case parsed of
      Left e -> ParseError e
      Right config -> config
