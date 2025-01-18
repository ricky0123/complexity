module Worker.Simulation where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExcept)
import Control.Monad.State
import Foreign (readInt)
import Web.Worker.GlobalScope (postMessage, onMessage)
import Web.Worker.MessageEvent (data_)
import Effect.Console (log)
import BrainFuck.Program
import Effect.Aff
import Effect.Aff.Class
import Control.Monad.Rec.Class (forever)
import Data.Tuple
import Data.Function (flip)
import Effect.Ref as Ref
import Web.Worker.MessageEvent (data_)
import Data.Maybe
import Foreign (Foreign, ForeignError, readInt)
import Foreign.Index ((!))
import Control.Monad.Except (Except, runExcept)
import Data.List.NonEmpty (NonEmptyList)
import Data.Either (fromRight)
import BrainFuck.Messages
import Data.Array (updateAt)
import Control.Monad.Maybe.Trans
import Control.Monad.Except.Trans
import Data.Array
import Effect.Now (now)
import Data.DateTime.Instant (Instant, diff)

type SimulationState =
  { population :: Array BrainFuckTape
  , programLength :: Int
  , populationSize :: Int
  , nInteractions :: Int
  , lastComputations :: Int
  , maxComputations :: Int
  , recordPrograms :: RecordPrograms
  }

readConfigMessage :: Foreign -> Except (NonEmptyList ForeignError) SimulationConfig
readConfigMessage value = do
  programLength <- value ! "programLength" >>= readInt
  populationSize <- value ! "populationSize" >>= readInt
  pure { programLength, populationSize }

main :: Effect Unit
main = launchAff_ do
  liftEffect $ onMessage \ev -> do
    let
      config = runExcept $ readConfigMessage $ data_ ev
    case config of
      Left e -> log $ show e
      Right { populationSize, programLength } -> do
        population <- initializeSoup populationSize programLength
        (state :: Ref.Ref SimulationState) <- Ref.new
          { population
          , programLength
          , populationSize
          , nInteractions: 0
          , lastComputations: 0
          , maxComputations: 0
          , recordPrograms: []
          }

        startTime <- now
        (updateTimer :: Ref.Ref Instant) <- Ref.new startTime
        log "Launching simulation"
        launchAff_ $ forever do
          currentState <- liftEffect $ Ref.read state
          maybeNewState <- liftEffect $ runInteraction currentState
          liftEffect $ case maybeNewState of
            Nothing -> log "Error in interaction"
            Just newState -> do
              flip Ref.write state $ currentState
                { nInteractions = currentState.nInteractions + 1
                , population = newState.population
                , lastComputations = newState.lastComputations
                , recordPrograms = newState.recordPrograms
                , maxComputations = newState.maxComputations
                }
              lastUpdateTime <- Ref.read updateTimer
              currentTime <- now
              let
                elapsed = diff currentTime lastUpdateTime
              when (elapsed > Milliseconds 400.0) do
                Ref.write currentTime updateTimer
                let
                  (update :: StatusUpdate) =
                    { nInteractions: currentState.nInteractions
                    , lastComputations: currentState.lastComputations
                    , recordPrograms: currentState.recordPrograms
                    }
                postMessage update

runInteraction :: SimulationState -> Effect (Maybe SimulationState)
runInteraction st = do
  maybeX <- getRandomProgram st.population
  maybeY <- getRandomProgram st.population
  let
    (result :: Maybe SimulationState) = do
      Tuple iX pX <- maybeX
      Tuple iY pY <- maybeY
      Tuple newPX newPY <- brainFuck pX pY
      newPopulation <- updateAt iX newPX st.population
      newPopulation <- updateAt iY newPY newPopulation
      let
        nComputationsX = countComputations newPX
        nComputationsY = countComputations newPY
        maxComputations = max (max nComputationsX nComputationsY) st.maxComputations
        recordProgramsX =
          if (nComputationsX > st.maxComputations) then
            cons { program: newPX, nComputations: nComputationsX, nInteractions: st.nInteractions } st.recordPrograms
          else
            st.recordPrograms
        recordPrograms =
          if (nComputationsY > st.maxComputations) then
            cons { program: newPY, nComputations: nComputationsY, nInteractions: st.nInteractions } recordProgramsX
          else
            recordProgramsX

      pure st
        { population = newPopulation
        , lastComputations = nComputationsX
        , maxComputations = maxComputations
        , recordPrograms = recordPrograms
        }
  pure result
