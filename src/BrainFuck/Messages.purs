module BrainFuck.Messages where

import BrainFuck.Program
import Data.Maybe (Maybe)
import Prelude

type SimulationConfig =
  { populationSize :: Int
  , programLength :: Int
  }

type RecordPrograms = Array RecordProgram
type RecordProgram = { program :: BrainFuckTape, nComputations :: Int, nInteractions :: Int }

type StatusUpdate =
  { nInteractions :: Int
  , lastComputations :: Int
  , recordPrograms :: RecordPrograms
  }
