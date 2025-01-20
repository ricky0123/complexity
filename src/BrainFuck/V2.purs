module BrainFuck.V2
  ( ArrayProgram(..)
  , Instruction(..)
  , class Program
  , copyReadHeadToWriteHead
  , copyWriteHeadToReadHead
  , decrement
  , decrementAtReadHead
  , executeStep
  , getInstruction
  , getRandomProgram
  , increment
  , incrementAtReadHead
  , loopEnd
  , loopStart
  , moveReadHeadLeft
  , moveReadHeadRight
  , moveWriteHeadLeft
  , moveWriteHeadRight
  , parseFromString
  , size
  , updateInstructionAt
  , writeToString
  , zero
  ) where

import Data.Array (length, unsafeIndex, updateAt)
import Effect (Effect)
import Prelude
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust)
import Data.String as String
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.String.CodePoints (codePointFromChar, fromCodePointArray)
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import Effect.Random (randomInt)

newtype Instruction = Instruction Int

derive instance eqInstruction :: Eq Instruction

instance showInstruction :: Show Instruction where
  show (Instruction i)
    | i < firstPrintableAscii = "\\" <> show i
    | i > lastPrintableAscii = "\\" <> show i
    | otherwise = unsafePartial $ fromJust $ fromCodePointArray <$> (pure <$> toEnum i)

firstPrintableAscii ∷ Int
firstPrintableAscii = fromEnum $ codePointFromChar '!'

lastPrintableAscii ∷ Int
lastPrintableAscii = fromEnum $ codePointFromChar '~'

moveReadHeadLeft :: Instruction
moveReadHeadLeft = Instruction 60

moveReadHeadRight :: Instruction
moveReadHeadRight = Instruction 62

moveWriteHeadLeft :: Instruction
moveWriteHeadLeft = Instruction 123

moveWriteHeadRight :: Instruction
moveWriteHeadRight = Instruction 125

decrementAtReadHead :: Instruction
decrementAtReadHead = Instruction 45

incrementAtReadHead :: Instruction
incrementAtReadHead = Instruction 43

copyReadHeadToWriteHead :: Instruction
copyReadHeadToWriteHead = Instruction 46

copyWriteHeadToReadHead :: Instruction
copyWriteHeadToReadHead = Instruction 44

loopStart :: Instruction
loopStart = Instruction 91

loopEnd :: Instruction
loopEnd = Instruction 93

zero :: Instruction
zero = Instruction 0

increment :: Instruction -> Instruction
increment (Instruction 127) = Instruction 0
increment (Instruction i) = Instruction (i + 1)

decrement :: Instruction -> Instruction
decrement (Instruction 0) = Instruction 127
decrement (Instruction i) = Instruction (i - 1)

class Program a where
  getInstruction :: a -> Int -> Instruction
  parseFromString :: String -> Maybe a
  writeToString :: a -> String
  getRandomProgram :: Int -> Effect a
  updateInstructionAt :: a -> Int -> Instruction -> a
  size :: a -> Int

newtype ArrayProgram = ArrayProgram (Array Instruction)

instance showArrayProgram :: Show ArrayProgram where
  show (ArrayProgram program) = "[ " <> (String.joinWith ", ") (map show program) <> " ]"

instance programArrayProgram :: Program ArrayProgram where
  getInstruction (ArrayProgram program) i = unsafePartial $ unsafeIndex program i

  parseFromString s = ArrayProgram <$> map (map Instruction) ascii
    where
    {-

    Instruction :: Int -> Instruction
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap :: (a -> b) -> Array a -> Array b

    F = fmap Instruction :: Array Int -> Array Instruction
    G = fmap G :: Maybe (Array Int) -> Maybe (Array Instruction)
      = fmap (fmap Instruction)

    -}

    ascii :: Maybe (Array Int)
    ascii = traverse _parseElement components

    components :: Array String
    components = String.split (String.Pattern " ") s

    _parseElement :: String -> Maybe Int
    _parseElement elem
      | String.length elem == 1 = fromEnum <$> String.codePointAt 0 elem
      | String.codePointAt 0 elem == Just _backslash = do
          numString <- String.stripPrefix (String.Pattern "\\") elem
          Int.fromString numString
      | otherwise = Nothing
    _backslash = codePointFromChar '\\'

  writeToString (ArrayProgram program) = String.joinWith " " $ map show program

  getRandomProgram size = ArrayProgram <$> replicateA size (Instruction <$> randomInt 0 128)
  updateInstructionAt (ArrayProgram program) i instruction = ArrayProgram $ unsafePartial $ fromJust $ updateAt i instruction program
  size (ArrayProgram program) = length program

type ExecutionContext a =
  { program :: a
  , instructionPointer :: Int
  , readHead :: Int
  , writeHead :: Int
  , nStepsTaken :: Int
  , maxSteps :: Int
  , done :: Boolean
  }

pred :: Int -> Int -> Int -> Int
pred min max i
  | i > min = i - 1
  | otherwise = max

succ :: Int -> Int -> Int -> Int
succ min max i
  | i < max = i + 1
  | otherwise = min

executeStep :: forall a. Program a => ExecutionContext a -> ExecutionContext a
executeStep ctx@{ done: true } = ctx
executeStep ctx@{ program, maxSteps, instructionPointer, readHead, writeHead, nStepsTaken } =
  let
    lastPointerPosition = size program - 1
    predPointer = pred 0 lastPointerPosition
    succPointer = succ 0 lastPointerPosition
    stepsIncremented = nStepsTaken + 1
    maxStepsReached = stepsIncremented == maxSteps
    atEndOfTape = instructionPointer == lastPointerPosition
    instruction = getInstruction program instructionPointer
    readChar = getInstruction program readHead
    writeChar = getInstruction program writeHead
  in
    case instruction of
      _
        | instruction == moveReadHeadLeft ->
            ctx
              { readHead = predPointer readHead
              , instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }

        | instruction == moveReadHeadRight ->
            ctx
              { readHead = succPointer readHead
              , instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }

        | instruction == moveWriteHeadLeft ->
            ctx
              { writeHead = predPointer writeHead
              , instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }

        | instruction == moveWriteHeadRight ->
            ctx
              { writeHead = succPointer writeHead
              , instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }

        | instruction == decrementAtReadHead ->
            let
              newProgram = updateInstructionAt program readHead (decrement readChar)
            in
              ctx
                { program = newProgram
                , instructionPointer = succPointer instructionPointer
                , nStepsTaken = stepsIncremented
                , done = atEndOfTape || maxStepsReached
                }

        | instruction == incrementAtReadHead ->
            let
              newProgram = updateInstructionAt program readHead (increment readChar)
            in
              ctx
                { program = newProgram
                , instructionPointer = succPointer instructionPointer
                , nStepsTaken = stepsIncremented
                , done = atEndOfTape || maxStepsReached
                }

        | instruction == copyReadHeadToWriteHead ->
            let
              newProgram = updateInstructionAt program writeHead readChar
            in
              ctx
                { program = newProgram
                , instructionPointer = succPointer instructionPointer
                , nStepsTaken = stepsIncremented
                , done = atEndOfTape || maxStepsReached
                }

        | instruction == copyWriteHeadToReadHead ->
            let
              newProgram = updateInstructionAt program readHead writeChar
            in
              ctx
                { program = newProgram
                , instructionPointer = succPointer instructionPointer
                , nStepsTaken = stepsIncremented
                , done = atEndOfTape || maxStepsReached
                }

        | instruction == loopStart ->
            if readChar /= zero then ctx
              { instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }
            else
              let
                (maybeNewInstructionPointer :: Maybe Int) = jumpForwardToMatchingBracket program instructionPointer

              in
                case maybeNewInstructionPointer of
                  Nothing ->
                    ctx
                      { instructionPointer = 0
                      , nStepsTaken = stepsIncremented
                      , done = true
                      }
                  Just newInstructionPointer ->
                    ctx
                      { instructionPointer = newInstructionPointer
                      , nStepsTaken = stepsIncremented
                      , done = maxStepsReached
                      }

        | instruction == loopEnd ->
            if readChar == zero then ctx
              { instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }
            else
              let
                (maybeNewInstructionPointer :: Maybe Int) = jumpBackwardToMatchingBracket program instructionPointer

              in
                case maybeNewInstructionPointer of
                  Nothing ->
                    ctx
                      { instructionPointer = 0
                      , nStepsTaken = stepsIncremented
                      , done = true
                      }
                  Just newInstructionPointer ->
                    ctx
                      { instructionPointer = newInstructionPointer
                      , nStepsTaken = stepsIncremented
                      , done = maxStepsReached
                      }

        | otherwise ->
            ctx
              { instructionPointer = succPointer instructionPointer
              , nStepsTaken = stepsIncremented
              , done = atEndOfTape || maxStepsReached
              }

jumpForwardToMatchingBracket :: forall a. Program a => a -> Int -> Maybe Int
jumpForwardToMatchingBracket = _jumpForwardToMatchingBracket (-1)

_jumpForwardToMatchingBracket :: forall a. Program a => Int -> a -> Int -> Maybe Int
_jumpForwardToMatchingBracket depth program pointer =
  let
    currentChar = getInstruction program pointer

  in
    case unit of
      _
        | (currentChar == loopEnd && depth > 0) ->
            _jumpForwardToMatchingBracket (depth - 1) program (pointer + 1)
        | (currentChar == loopEnd && depth == 0) ->
            pure pointer
        | (currentChar == loopStart) ->
            _jumpForwardToMatchingBracket (depth + 1) program (pointer + 1)
        | otherwise ->
            _jumpForwardToMatchingBracket depth program (pointer + 1)

jumpBackwardToMatchingBracket :: forall a. Program a => a -> Int -> Maybe Int
jumpBackwardToMatchingBracket = _jumpBackwardToMatchingBracket (-1)

_jumpBackwardToMatchingBracket :: forall a. Program a => Int -> a -> Int -> Maybe Int
_jumpBackwardToMatchingBracket depth program pointer =
  let
    currentChar = getInstruction program pointer
  in
    case unit of
      _
        | (currentChar == loopStart && depth > 0) ->
            _jumpBackwardToMatchingBracket (depth - 1) program (pointer - 1)
        | (currentChar == loopStart && depth == 0) ->
            pure pointer
        | (currentChar == loopEnd) ->
            _jumpBackwardToMatchingBracket (depth + 1) program (pointer - 1)
        | otherwise ->
            _jumpBackwardToMatchingBracket depth program (pointer - 1)
