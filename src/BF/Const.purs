module BF.Const where

import Prelude

import Effect (Effect)
import Effect.Console (log)

defaultNPrograms ∷ Int
defaultNPrograms = 131072

defaultIterations ∷ Int
defaultIterations = 20_000

defaultMaxSteps ∷ Int
defaultMaxSteps = 8192

defaultProgramLength :: Int
defaultProgramLength = 64

readHeadBackward = 60
readHeadForward = 62
writeHeadBackward = 123
writeHeadForward = 125
decrementAtReadHead = 45
incrementAtReadHead = 43
copyReadToWrite = 46
copyWriteToRead = 44
loopStart = 91
loopEnd = 93
loopTestNull = 0

nonNoOps =
  [ readHeadBackward
  , readHeadForward
  , writeHeadBackward
  , writeHeadForward
  , decrementAtReadHead
  , incrementAtReadHead
  , copyReadToWrite
  , copyWriteToRead
  , loopStart
  , loopEnd
  , loopTestNull
  ]
