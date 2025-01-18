module BrainFuck.Examples where

import BrainFuck.Program (BrainFuckTape, parseBrainFuckProgram)

import Data.Maybe (fromMaybe)

import Prelude

someTape :: BrainFuckTape
someTape = fromMaybe [] $ parseBrainFuckProgram selfReplicatingTape

selfReplicatingTape :: String
selfReplicatingTape = "[ [ { . > ] - ] a b c d e f g h i j ] - ] > . { [ [ \\0 \\1 \\2 \\3 \\4 \\5 \\6 \\7 \\8 \\9 \\10 \\11 \\12 \\13 \\14 \\15 \\16 \\17 \\18 \\19 \\20 \\21 \\22 \\23 \\24"
