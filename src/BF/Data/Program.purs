module BF.Data.Program where

import Prelude

import BF.Const
  ( copyReadToWrite
  , copyWriteToRead
  , decrementAtReadHead
  , defaultNPrograms
  , incrementAtReadHead
  , loopEnd
  , loopStart
  , loopTestNull
  , readHeadBackward
  , readHeadForward
  , writeHeadBackward
  , writeHeadForward
  )
import BF.Data.ArrayView
  ( Uint8Array
  , _indexUint8Array
  , decAtI
  , fromArray
  , incAtI
  , index
  , toArray
  , writeAt
  )
import BF.Data.Utils (foldMapUint8Array)
import Control.Monad.Rec.Class (Step(..), tailRec, tailRecM)
import Data.Array (length, singleton, splitAt)
import Data.DateTime.Instant (Instant, diff)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Duration (class Duration, Seconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Halogen.Subscription as HS
import Log (_debug, getTimeElapsedString)
import Web.Worker.Worker
  ( Worker
  , defaultWorkerOptions
  , new
  , onMessage
  , postMessage
  , terminate
  )

type BFFields =
  { firstTape :: Uint8Array, secondTape :: Uint8Array, cut :: Int, size :: Int }

newtype BF = BF BFFields

derive instance Newtype BF _
derive instance Eq BF

instance showBF :: Show BF where
  show = show <<< programToArray

_cut :: BF -> Int
_cut = _.cut <<< unwrap

_size :: BF -> Int
_size = _.size <<< unwrap

_firstTape :: BF -> Uint8Array
_firstTape = _.firstTape <<< unwrap

_secondTape :: BF -> Uint8Array
_secondTape = _.secondTape <<< unwrap

modifyAtIndex :: forall a. (Uint8Array -> Int -> a) -> BF -> Int -> a
modifyAtIndex f (BF { firstTape, secondTape, cut }) i
  | i < cut = f firstTape i
  | otherwise = f secondTape $ i - cut

elemAtI :: BF -> Int -> Int
elemAtI = modifyAtIndex index

infix 6 elemAtI as !!

incAtI' :: BF -> Int -> Effect Unit
incAtI' = modifyAtIndex incAtI

decAtI' :: BF -> Int -> Effect Unit
decAtI' = modifyAtIndex decAtI

copyProgramValue :: BF -> Int -> Int -> Effect Unit
copyProgramValue bf@(BF { firstTape, secondTape, cut }) src tgt
  | tgt < cut = writeAt firstTape tgt (bf !! src)
  | otherwise = writeAt secondTape (tgt - cut) (bf !! src)

type BFExecutionContext =
  { nStepsTaken :: Int
  , maxSteps :: Int
  , instruction :: Int
  , instructionHead :: Int
  , readHead :: Int
  , writeHead :: Int
  , program :: BF
  }

runBF :: Int -> BF -> Effect BF
runBF maxSteps program =
  tailRecM
    stepBF
    { nStepsTaken: 0
    , maxSteps
    , instruction: program !! 0
    , instructionHead: 0
    , readHead: 0
    , writeHead: 0
    , program: program
    }

stepBF :: BFExecutionContext -> Effect (Step BFExecutionContext BF)
stepBF ctx@{ instruction, readHead, writeHead, program }
  | instruction == readHeadBackward = pure $ advanceNormal $ ctx
      { readHead = decHead (_size program) readHead }
  | instruction == readHeadForward = pure $ advanceNormal $ ctx
      { readHead = incHead (_size program) readHead }
  | instruction == writeHeadBackward = pure $ advanceNormal $ ctx
      { writeHead = decHead (_size program) writeHead }
  | instruction == writeHeadForward = pure $ advanceNormal $ ctx
      { writeHead = incHead (_size program) writeHead }
  | instruction == decrementAtReadHead = advanceNormal ctx <$ decAtI' program readHead
  | instruction == incrementAtReadHead = advanceNormal ctx <$ incAtI' program readHead
  | instruction == copyReadToWrite = advanceNormal ctx <$ copyProgramValue program
      readHead
      writeHead
  | instruction == copyWriteToRead = advanceNormal ctx <$ copyProgramValue program
      writeHead
      readHead
  | instruction == loopStart = pure $ advanceLoopStart ctx
  | instruction == loopEnd = pure $ advanceLoopEnd ctx
  | otherwise = pure $ advanceNormal ctx

---- 
advanceNormal :: BFExecutionContext -> (Step BFExecutionContext BF)
advanceNormal ctx@({ instructionHead, nStepsTaken, program, maxSteps })
  | nStepsTaken >= maxSteps - 1 = Done program
  | instructionHead == _size program - 1 = Done program
  | otherwise =
      let
        newInstructionHead = instructionHead + 1
      in
        Loop $ ctx
          { nStepsTaken = nStepsTaken + 1
          , instructionHead = newInstructionHead
          , instruction = program !! newInstructionHead
          }

advanceLoopStart :: BFExecutionContext -> Step BFExecutionContext BF
advanceLoopStart
  ctx@{ instructionHead, instruction, nStepsTaken, program, readHead, maxSteps }
  | nStepsTaken >= maxSteps - 1 = Done program
  | program !! readHead /= loopTestNull = advanceNormal ctx
  | otherwise =
      case (jumpForwardToLoopEnd program instructionHead instruction) of
        Nothing -> Done program
        Just res ->
          Loop ctx
            { instruction = res.instruction, instructionHead = res.instructionHead }

jumpForwardToLoopEnd
  :: BF -> Int -> Int -> Maybe { instruction :: Int, instructionHead :: Int }
jumpForwardToLoopEnd program startInstructionHead startInstruction =
  tailRec go
    { depth: -1, instruction: startInstruction, instructionHead: startInstructionHead }
  where
  go :: _ -> Step _ (Maybe _)
  go loopCtx@{ depth, instruction, instructionHead }
    | instructionHead == _size program - 1 = Done Nothing
    | instruction == loopEnd && depth == 0 = Done $ Just
        { instruction
        , instructionHead
        }
    | instruction == loopEnd && depth > 0 = Loop $
        loopCtx
          { depth = depth - 1
          , instructionHead = instructionHead + 1
          , instruction = program !! (instructionHead + 1)
          }
    | instruction == loopStart = Loop $
        loopCtx
          { depth = depth + 1
          , instructionHead = instructionHead + 1
          , instruction = program !! (instructionHead + 1)
          }
    | otherwise = Loop $
        loopCtx
          { instructionHead = instructionHead + 1
          , instruction = program !! (instructionHead + 1)
          }

advanceLoopEnd :: BFExecutionContext -> Step BFExecutionContext BF
advanceLoopEnd
  ctx@{ instructionHead, instruction, nStepsTaken, program, readHead, maxSteps }
  | nStepsTaken >= maxSteps - 1 = Done program
  | program !! readHead == loopTestNull = advanceNormal ctx
  | otherwise =
      case (jumpBackToLoopStart program instructionHead instruction) of
        Nothing -> Done program
        Just res ->
          Loop ctx
            { instruction = res.instruction, instructionHead = res.instructionHead }

jumpBackToLoopStart
  :: BF -> Int -> Int -> Maybe { instruction :: Int, instructionHead :: Int }
jumpBackToLoopStart program startInstructionHead startInstruction =
  tailRec go
    { depth: -1, instruction: startInstruction, instructionHead: startInstructionHead }
  where
  go :: _ -> Step _ (Maybe _)
  go loopCtx@{ depth, instruction, instructionHead }
    | instruction == loopStart && depth == 0 = Done $ Just
        { instruction
        , instructionHead
        }
    | instructionHead == 0 = Done Nothing
    | instruction == loopStart = Loop $
        loopCtx
          { depth = depth - 1
          , instructionHead = instructionHead - 1
          , instruction = program !! (instructionHead - 1)
          }
    | instruction == loopEnd = Loop $
        loopCtx
          { depth = depth + 1
          , instructionHead = instructionHead - 1
          , instruction = program !! (instructionHead - 1)
          }
    | otherwise = Loop $
        loopCtx
          { instructionHead = instructionHead - 1
          , instruction = program !! (instructionHead - 1)
          }

-- utils
decHead :: Int -> Int -> Int
decHead size i
  | i <= 0 = size - 1
  | otherwise = i - 1

incHead :: Int -> Int -> Int
incHead size i
  | i >= size - 1 = 0
  | otherwise = i + 1

arrayToProgram :: Int -> Array Int -> BF
arrayToProgram cut arr = BF { firstTape, secondTape, cut, size: length arr }
  where
  { before, after } = splitAt cut arr
  firstTape = fromArray before
  secondTape = fromArray after

programToArray :: BF -> Array Int
programToArray (BF { firstTape, secondTape }) =
  toArray firstTape <> toArray secondTape
