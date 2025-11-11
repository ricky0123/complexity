module Test.Main where

import Prelude
import Test.Spec
import Test.Spec.Assertions
import Test.Spec.Reporter
import Test.Spec.Runner.Node

import BF.Const
  ( copyReadToWrite
  , copyWriteToRead
  , decrementAtReadHead
  , incrementAtReadHead
  , loopEnd
  , loopStart
  , loopTestNull
  , readHeadBackward
  , readHeadForward
  , writeHeadBackward
  , writeHeadForward
  )
import BF.Data.ArrayView (Float32Array, Int32Array, Uint32Array, Uint8Array, fromArray)
import BF.Data.Program (BF, arrayToProgram, programToArray, runBF)
import BF.Data.Utils (bufferOf)
import BF.Metrics (countNoOps)
import BF.Simulation (getWorkerPermutationEndpoints)
import BF.SimulationConfig
  ( ConfigParseResult(..)
  , WorkerConfigMessage
  , parseWorkerConfigMessage
  )
import Data.Array (length, (..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Log (_debug)
import Record as Record
import Type.Proxy (Proxy(..))

debug = _debug "test"

genericNoOp = 200

_createBFTestProgram :: Array Int -> Array Int -> Int -> BF
_createBFTestProgram prefix suffix fill = arrayToProgram (length prefix) $ prefix
  <> [ fill ]
  <> suffix

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

isParseError :: forall a. ConfigParseResult a -> Boolean
isParseError (ParseError _) = true
isParseError _ = false

spec :: Spec Unit
spec = do
  it "fail to parse empty config message" do
    let
      result = parseWorkerConfigMessage $ unsafeToForeign {}
    isParseError result `shouldEqual` true

  it "parse worker config message" do
    let
      message =
        { nPrograms: 20
        , programLength: 10
        , soup: bufferOf $ (fromArray [ 100 ] :: Uint8Array)
        , metricsBuffers:
            { interactionCounterBuffer: bufferOf $ (fromArray [ 1 ] :: Uint32Array)
            , interactionMetricRecordBuffer: bufferOf $ (fromArray [ 2 ] :: Uint32Array)
            , noOpMetricRecordBuffer: bufferOf $ (fromArray [ 3 ] :: Uint32Array)
            }
        , permutationBuffer: bufferOf $ (fromArray [ 4 ] :: Uint32Array)
        , masterCommandBuffer: bufferOf $ (fromArray [ 5 ] :: Int32Array)
        , workerStatusBuffer: bufferOf $ (fromArray [ 6 ] :: Int32Array)
        , isMaster: true
        , workerIndex: 4
        , nWorkers: 1
        } :: WorkerConfigMessage
    let
      result = parseWorkerConfigMessage $ unsafeToForeign message

    result `shouldEqual` ParsedMaster
      { nPrograms: 20
      , programLength: 10
      , soup: bufferOf $ (fromArray [ 100 ] :: Uint8Array)
      , metrics:
          { interactionCounter: fromArray [ 1 ]
          , interactionMetricRecord: fromArray [ 2 ]
          , noOpMetricRecord: fromArray [ 3 ]
          }
      , permutations: fromArray [ 4 ]
      , masterCommand: fromArray [ 5 ]
      , workerStatus: fromArray [ 6 ]
      , workerIndex: 4
      , nWorkers: 1
      }
    let
      nPrograms_ = Proxy :: Proxy "nPrograms"
      badMessage = unsafeToForeign $ Record.delete nPrograms_ message
      badResult = parseWorkerConfigMessage badMessage

    isParseError badResult `shouldEqual` true

  it "count no ops" do
    countNoOps (fromArray [ genericNoOp, decrementAtReadHead, 77, copyReadToWrite, 72 ])
      `shouldEqual` 3

  for_
    [ { name: "simple"
      , nWorkers: 4
      , nPrograms: 20
      , expected: [ Tuple 0 4, Tuple 5 9, Tuple 10 14, Tuple 15 19 ]
      }
    , { name: "larger numbers"
      , nWorkers: 6
      , nPrograms: 182
      , expected:
          [ Tuple 0 29
          , Tuple 30 59
          , Tuple 60 89
          , Tuple 90 119
          , Tuple 120 149
          , Tuple 150 181
          ]
      }
    , { name: "larger-er numbers"
      , nWorkers: 6
      , nPrograms: 300_000
      , expected:
          [ Tuple 0 49_999
          , Tuple 50_000 99_999
          , Tuple 100_000 149_999
          , Tuple 150_000 199_999
          , Tuple 200_000 249_999
          , Tuple 250_000 299_999
          ]
      }
    ]
    \t -> do
      it ("getWorkerPermutationEndpoints: " <> t.name) do
        let
          result =
            getWorkerPermutationEndpoints t.nWorkers t.nPrograms
              <$> 0 .. (t.nWorkers - 1)
        result `shouldEqual` t.expected

  for_
    [ { name: "simple increment"
      , input: _createBFTestProgram [ incrementAtReadHead ] [] genericNoOp
      , expected: _createBFTestProgram [ incrementAtReadHead + 1 ] [] genericNoOp
      }
    , { name: "move read head and increment"
      , input: _createBFTestProgram [ readHeadForward, incrementAtReadHead ] []
          genericNoOp
      , expected: _createBFTestProgram [ readHeadForward, incrementAtReadHead + 1 ] []
          genericNoOp
      }
    , { name: "decrement"
      , input: _createBFTestProgram [ readHeadForward, decrementAtReadHead ] []
          genericNoOp
      , expected: _createBFTestProgram [ readHeadForward, decrementAtReadHead - 1 ] []
          genericNoOp
      }
    , { name: "enter loop"
      , input: _createBFTestProgram
          [ loopTestNull + 1, loopStart, decrementAtReadHead, loopEnd ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ loopTestNull, loopStart, decrementAtReadHead, loopEnd ]
          []
          genericNoOp
      }
    , { name: "dont enter loop"
      , input: _createBFTestProgram
          [ loopTestNull, loopStart, decrementAtReadHead, loopEnd ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ loopTestNull, loopStart, decrementAtReadHead, loopEnd ]
          []
          genericNoOp
      }
    , { name: "wrap around"
      , input: _createBFTestProgram
          [ loopTestNull
          , loopTestNull
          , readHeadForward
          , writeHeadBackward
          , copyWriteToRead
          , genericNoOp
          ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ loopTestNull
          , genericNoOp
          , readHeadForward
          , writeHeadBackward
          , copyWriteToRead
          , genericNoOp
          ]
          []
          genericNoOp
      }
    , { name: "non trivial loop"
      , input: _createBFTestProgram
          [ readHeadForward
          , loopTestNull + 3
          , loopStart
          , genericNoOp
          , decrementAtReadHead
          , loopEnd
          , genericNoOp
          ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ readHeadForward
          , loopTestNull
          , loopStart
          , genericNoOp
          , decrementAtReadHead
          , loopEnd
          , genericNoOp
          ]
          []
          genericNoOp
      }
    , { name: "don't get stuck in infinite loop"
      , input: _createBFTestProgram
          [ loopStart, loopEnd ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ loopStart, loopEnd ]
          []
          genericNoOp
      }
    , { name: "increment ascii wraps around"
      , input: _createBFTestProgram
          [ genericNoOp, loopStart, incrementAtReadHead, loopEnd ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ loopTestNull, loopStart, incrementAtReadHead, loopEnd ]
          []
          genericNoOp
      }
    , { name: "decrement ascii wraps around"
      , input: _createBFTestProgram
          [ readHeadBackward, readHeadForward, loopStart, decrementAtReadHead, loopEnd ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ loopTestNull, readHeadForward, loopStart, decrementAtReadHead, loopEnd ]
          []
          genericNoOp
      }
    , { name: "random test 1"
      , input: _createBFTestProgram
          [ 1
          , loopStart
          , writeHeadBackward
          , copyReadToWrite
          , decrementAtReadHead
          , loopEnd
          ]
          [ 0 ]
          genericNoOp
      , expected: _createBFTestProgram
          [ 0
          , loopStart
          , writeHeadBackward
          , copyReadToWrite
          , decrementAtReadHead
          , loopEnd
          ]
          [ 1 ]
          genericNoOp
      }
    , { name: "fig 4 replicator"
      , input:
          arrayToProgram
            0
            [ loopStart
            , loopStart
            , writeHeadBackward
            , copyReadToWrite
            , readHeadForward
            , loopEnd
            , decrementAtReadHead
            , loopEnd
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , loopEnd
            , decrementAtReadHead
            , loopEnd
            , readHeadForward
            , copyReadToWrite
            , writeHeadBackward
            , loopStart
            , loopStart
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]
      , expected:
          arrayToProgram
            0
            [ loopStart
            , loopStart
            , writeHeadBackward
            , copyReadToWrite
            , readHeadForward
            , loopEnd
            , decrementAtReadHead
            , loopEnd
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , loopEnd
            , decrementAtReadHead
            , loopEnd
            , readHeadForward
            , copyReadToWrite
            , writeHeadBackward
            , loopStart
            , loopStart
            , loopStart
            , loopStart
            , writeHeadBackward
            , copyReadToWrite
            , readHeadForward
            , loopEnd
            , decrementAtReadHead
            , loopEnd
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , 105
            , loopEnd
            , decrementAtReadHead
            , loopEnd
            , readHeadForward
            , copyReadToWrite
            , writeHeadBackward
            , loopStart
            , loopStart
            ]
      }
    , { name: "don't execute instructions after infinite loop"
      , input: _createBFTestProgram
          [ genericNoOp, incrementAtReadHead, loopStart, loopEnd, incrementAtReadHead ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ genericNoOp + 1
          , incrementAtReadHead
          , loopStart
          , loopEnd
          , incrementAtReadHead
          ]
          []
          genericNoOp
      }
    , { name: "print H"
      , input: _createBFTestProgram
          [ writeHeadBackward
          , 0
          , 0
          , readHeadForward
          , readHeadForward
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , loopStart
          , readHeadBackward
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , readHeadForward
          , decrementAtReadHead
          , loopEnd
          , readHeadBackward
          , copyReadToWrite
          ]
          [ 0 ]
          genericNoOp
      , expected: _createBFTestProgram
          [ writeHeadBackward
          , 72
          , 0
          , readHeadForward
          , readHeadForward
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , loopStart
          , readHeadBackward
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , incrementAtReadHead
          , readHeadForward
          , decrementAtReadHead
          , loopEnd
          , readHeadBackward
          , copyReadToWrite
          ]
          [ 72 ]
          genericNoOp
      }
    , { name: "nested loop 1"
      , input: _createBFTestProgram
          [ 4
          , 4
          , 4
          , 0
          , loopStart
          , loopStart
          , decrementAtReadHead
          , loopEnd
          , readHeadForward
          , loopEnd
          , 1
          ]
          []
          genericNoOp
      , expected: _createBFTestProgram
          [ 0
          , 0
          , 0
          , 0
          , loopStart
          , loopStart
          , decrementAtReadHead
          , loopEnd
          , readHeadForward
          , loopEnd
          , 1
          ]
          []
          genericNoOp
      }
    , { name: "max steps 2"
      , input: _createBFTestProgram
          [ writeHeadBackward, loopStart, loopEnd, copyReadToWrite ]
          [ 0 ]
          genericNoOp
      , expected: _createBFTestProgram
          [ writeHeadBackward, loopStart, loopEnd, copyReadToWrite ]
          [ 0 ]
          genericNoOp
      }
    ]
    \t -> do
      it ("execute " <> t.name) do
        result <- liftEffect $ runBF 1000 t.input
        result `shouldEqual` t.expected
