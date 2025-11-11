module Worker.Simulation where

import Prelude

import BF.Const (defaultIterations, defaultNPrograms)
import BF.Data.ArrayView (BigInt64Array, SharedArrayBuffer)
import BF.Metrics (metricsFromBuffers)
import BF.Permutation (getPermutationArray)
import BF.Signals (getMasterCommandArray, getWorkerStatusArray)
import BF.Simulation (startMasterWorker, startNonMasterWorker)
import BF.SimulationConfig (ConfigParseResult(..), parseWorkerConfigMessage)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Rec.Class (class MonadRec, Step(..), forever, tailRecM)
import Data.DateTime.Instant (Instant, diff)
import Data.Either (Either(..))
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (class Duration, Seconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Random (randomInt)
import Foreign (Foreign, ForeignError, readInt, unsafeFromForeign)
import Foreign.Index ((!))
import Log (_debug)
import Web.Worker.GlobalScope (onMessage)
import Web.Worker.MessageEvent (data_)

debug = _debug "simulation"

main :: Effect Unit
main = launchAff_ do
  liftEffect $ onMessage
    \ev ->
      case parseWorkerConfigMessage (data_ ev) of
        ParseError e -> debug e
        ParsedMaster masterConfig -> do
          debug "Starting master worker"
          startMasterWorker masterConfig
        ParsedNonMaster nonMasterConfig -> do
          debug "Starting non-master worker"
          startNonMasterWorker nonMasterConfig
