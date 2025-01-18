module BrainFuck.Program
  ( BrainFuckExecutionContext
  , BrainFuckTape
  , _jumpBackwardToMatchingBracket
  , _jumpForwardToMatchingBracket
  , brainFuck
  , comma
  , executeBrainFuckProgram
  , executeBrainFuckStep
  , firstPrintableAscii
  , getInitialBrainFuckExecutionContext
  , countComputations
  , getRandomProgram
  , getRandomTape
  , greaterThan
  , initializeSoup
  , jumpBackwardToMatchingBracket
  , jumpForwardToMatchingBracket
  , lastPrintableAscii
  , leftCurlyBrace
  , leftSquareBracket
  , lessThan
  , minus
  , parseBrainFuckProgram
  , period
  , plus
  , pred
  , predAscii
  , rightCurlyBrace
  , rightSquareBracket
  , succ
  , succAscii
  , unParseBrainFuckProgram
  ) where

import Data.Array
import Data.Enum
import Data.Maybe
import Effect
import Effect.Class
import Effect.Random (randomInt)
import Prelude

import Data.Char (fromCharCode)
import Data.Int as Int
import Data.String as S
import Data.String.CodePoints (codePointFromChar, toCodePointArray, fromCodePointArray)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import Data.Tuple

getRandomProgram :: Array BrainFuckTape -> Effect (Maybe (Tuple Int BrainFuckTape))
getRandomProgram population = do
  index <- randomInt 0 (length population - 1)
  let
    result = do
      program <- population !! index
      pure $ Tuple index program
  pure result

-- from two BrainFuckTapes, concatenate, run executation, and break apart
brainFuck :: BrainFuckTape -> BrainFuckTape -> Maybe (Tuple BrainFuckTape BrainFuckTape)
brainFuck p1 p2 = do
  let
    combined = p1 <> p2
    ctx = getInitialBrainFuckExecutionContext combined
  result <- executeBrainFuckProgram ctx
  let
    halfLength = length combined `div` 2
    firstHalf = take halfLength result.tape
    secondHalf = drop halfLength result.tape
  pure $ Tuple firstHalf secondHalf

-- from BrainFuckTape, count number of elements that are not no-ops
countComputations :: BrainFuckTape -> Int
countComputations tape = length $ filter isComputation tape
  where
  isComputation x = x == plus || x == minus || x == period || x == comma
    || x == leftSquareBracket
    || x == rightSquareBracket
    || x == lessThan
    || x == greaterThan

initializeSoup :: Int -> Int -> Effect (Array BrainFuckTape)
initializeSoup populationSize programLength = replicateA populationSize $ getRandomTape programLength

getRandomTape :: Int -> Effect BrainFuckTape
getRandomTape length = replicateA length (randomInt _minAscii _maxAscii)

-- simple way of writing ascii strings (including non-printable chars)
parseBrainFuckProgram :: String -> Maybe BrainFuckTape
parseBrainFuckProgram s = traverse _parseElement $ S.split (S.Pattern " ") s
  where
  _parseElement :: String -> Maybe Int
  _parseElement elem
    | S.length elem == 1 = fromEnum <$> S.codePointAt 0 elem
    | S.codePointAt 0 elem == Just _backslash = do
        numString <- S.stripPrefix (S.Pattern "\\") elem
        Int.fromString numString
    | otherwise = Nothing
  _backslash = codePointFromChar '\\'

unParseBrainFuckProgram :: BrainFuckTape -> Maybe String
unParseBrainFuckProgram tape = (S.joinWith " ") <$> (traverse _fmtInt tape)
  where
  _fmtInt :: Int -> Maybe String
  _fmtInt i
    | i < firstPrintableAscii = Just $ "\\" <> show i
    | i > lastPrintableAscii = Just $ "\\" <> show i
    | otherwise = fromCodePointArray <$> (pure <$> toEnum i)

firstPrintableAscii = fromEnum $ codePointFromChar '!'
lastPrintableAscii = fromEnum $ codePointFromChar '~'

leftSquareBracket :: Int
leftSquareBracket = fromEnum $ codePointFromChar '['

rightSquareBracket :: Int
rightSquareBracket = fromEnum $ codePointFromChar ']'

period :: Int
period = fromEnum $ codePointFromChar '.'

comma :: Int
comma = fromEnum $ codePointFromChar ','

leftCurlyBrace :: Int
leftCurlyBrace = fromEnum $ codePointFromChar '{'

rightCurlyBrace :: Int
rightCurlyBrace = fromEnum $ codePointFromChar '}'

lessThan :: Int
lessThan = fromEnum $ codePointFromChar '<'

greaterThan :: Int
greaterThan = fromEnum $ codePointFromChar '>'

minus :: Int
minus = fromEnum $ codePointFromChar '-'

plus :: Int
plus = fromEnum $ codePointFromChar '+'

pred :: Int -> Int -> Int -> Int
pred min max i
  | i > min = i - 1
  | otherwise = max

succ :: Int -> Int -> Int -> Int
succ min max i
  | i < max = i + 1
  | otherwise = min

_minAscii :: Int
_minAscii = 0

_maxAscii :: Int
_maxAscii = 127

predAscii ∷ Int → Int
predAscii = pred _minAscii _maxAscii

succAscii ∷ Int → Int
succAscii = succ _minAscii _maxAscii

type BrainFuckTape = Array Int

type BrainFuckExecutionContext =
  { tape :: BrainFuckTape
  , instructionPointer :: Int
  , readHead :: Int
  , writeHead :: Int
  , nStepsTaken :: Int
  , maxSteps :: Int
  , done :: Boolean
  }

getInitialBrainFuckExecutionContext :: BrainFuckTape -> BrainFuckExecutionContext
getInitialBrainFuckExecutionContext tape =
  { tape: tape
  , instructionPointer: 0
  , readHead: 0
  , writeHead: 0
  , nStepsTaken: 0
  , maxSteps: 1000
  , done: false
  }

executeBrainFuckProgram :: BrainFuckExecutionContext -> Maybe BrainFuckExecutionContext
executeBrainFuckProgram ctx =
  if ctx.done then pure ctx
  else do
    step <- executeBrainFuckStep ctx
    executeBrainFuckProgram step

-- Nothing corresponds to a crash, eg readHead negative or something
executeBrainFuckStep :: BrainFuckExecutionContext -> Maybe BrainFuckExecutionContext
executeBrainFuckStep ctx = do
  let
    { tape, maxSteps, instructionPointer, readHead, writeHead, nStepsTaken, done } = ctx
    size = length tape
    lastPointerPosition = size - 1
    predPointer = pred 0 lastPointerPosition
    succPointer = succ 0 lastPointerPosition
    stepsIncremented = nStepsTaken + 1
    maxStepsReached = stepsIncremented == maxSteps
    atEndOfTape = instructionPointer == lastPointerPosition

  when (nStepsTaken >= maxSteps) Nothing
  instruction <- tape !! instructionPointer
  readChar <- tape !! readHead
  writeChar <- tape !! writeHead

  case instruction of
    _
      | instruction == lessThan ->
          pure $ ctx
            { readHead = predPointer readHead
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == greaterThan ->
          pure $ ctx
            { readHead = succPointer readHead
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == leftCurlyBrace ->
          pure $ ctx
            { writeHead = predPointer writeHead
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == rightCurlyBrace ->
          pure $ ctx
            { writeHead = succPointer writeHead
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == minus -> do
          newTape <-
            updateAt readHead (predAscii readChar) tape
              :: Maybe (BrainFuckTape)
          pure $ ctx
            { tape = newTape
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == plus -> do
          newTape <-
            updateAt readHead (succAscii readChar) tape
              :: Maybe (BrainFuckTape)
          pure $ ctx
            { tape = newTape
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == period -> do
          newTape <-
            updateAt writeHead readChar tape
              :: Maybe (BrainFuckTape)
          pure $ ctx
            { tape = newTape
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == comma -> do
          newTape <-
            updateAt readHead writeChar tape
              :: Maybe (BrainFuckTape)
          pure $ ctx
            { tape = newTape
            , instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

      | instruction == leftSquareBracket ->
          if readChar /= 0 then pure $ ctx
            { instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }
          else
            let
              (maybeNewInstructionPointer :: Maybe Int) = jumpForwardToMatchingBracket tape instructionPointer

            in
              case maybeNewInstructionPointer of
                Nothing ->
                  pure $ ctx
                    { instructionPointer = 0
                    , nStepsTaken = stepsIncremented
                    , done = true
                    }
                Just newInstructionPointer ->
                  pure $ ctx
                    { instructionPointer = newInstructionPointer
                    , nStepsTaken = stepsIncremented
                    , done = maxStepsReached
                    }

      | instruction == rightSquareBracket ->
          if readChar == 0 then pure $ ctx
            { instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }
          else
            let
              (maybeNewInstructionPointer :: Maybe Int) = jumpBackwardToMatchingBracket tape instructionPointer

            in
              case maybeNewInstructionPointer of
                Nothing ->
                  pure $ ctx
                    { instructionPointer = 0
                    , nStepsTaken = stepsIncremented
                    , done = true
                    }
                Just newInstructionPointer ->
                  pure $ ctx
                    { instructionPointer = newInstructionPointer
                    , nStepsTaken = stepsIncremented
                    , done = maxStepsReached
                    }

      | otherwise ->
          pure $ ctx
            { instructionPointer = succPointer instructionPointer
            , nStepsTaken = stepsIncremented
            , done = atEndOfTape || maxStepsReached
            }

jumpForwardToMatchingBracket :: BrainFuckTape -> Int -> Maybe Int
jumpForwardToMatchingBracket = _jumpForwardToMatchingBracket (-1)

_jumpForwardToMatchingBracket :: Int -> BrainFuckTape -> Int -> Maybe Int
_jumpForwardToMatchingBracket depth tape pointer = do
  currentChar <- tape !! pointer
  case unit of
    _
      | (currentChar == rightSquareBracket && depth > 0) ->
          _jumpForwardToMatchingBracket (depth - 1) tape (pointer + 1)
      | (currentChar == rightSquareBracket && depth == 0) ->
          pure pointer
      | (currentChar == leftSquareBracket) ->
          _jumpForwardToMatchingBracket (depth + 1) tape (pointer + 1)
      | otherwise ->
          _jumpForwardToMatchingBracket depth tape (pointer + 1)

jumpBackwardToMatchingBracket :: BrainFuckTape -> Int -> Maybe Int
jumpBackwardToMatchingBracket = _jumpBackwardToMatchingBracket (-1)

_jumpBackwardToMatchingBracket :: Int -> BrainFuckTape -> Int -> Maybe Int
_jumpBackwardToMatchingBracket depth tape pointer = do
  currentChar <- tape !! pointer
  case unit of
    _
      | (currentChar == leftSquareBracket && depth > 0) ->
          _jumpBackwardToMatchingBracket (depth - 1) tape (pointer - 1)
      | (currentChar == leftSquareBracket && depth == 0) ->
          pure pointer
      | (currentChar == rightSquareBracket) ->
          _jumpBackwardToMatchingBracket (depth + 1) tape (pointer - 1)
      | otherwise ->
          _jumpBackwardToMatchingBracket depth tape (pointer - 1)
