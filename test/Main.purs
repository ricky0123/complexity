module Test.Main where

import Data.Foldable
import Data.Maybe
import Prelude
import Test.Spec
import Test.Spec.Assertions
import Test.Spec.Reporter
import Test.Spec.Runner.Node

import BrainFuck.Program (BrainFuckTape, executeBrainFuckProgram, getInitialBrainFuckExecutionContext, parseBrainFuckProgram, unParseBrainFuckProgram)
import Effect (Effect)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] spec



spec :: Spec Unit
spec = do
  describe "BrainFuck execution" do
    for_ [ 
      {
        name: "noop x",
        input: "x",
        output: "x"
      },
      {
        name: "noop long string",
        input: "a b c d e f g h i j k l m n o p",
        output: "a b c d e f g h i j k l m n o p"
      },
      {
        name: "decrement tape singleton",
        input: "-",
        output: ","
      },
      {
        name: "increment tape singleton",
        input: "+",
        output: ","
      },
      {
        name: "move read head and increment",
        input: "> c +",
        output: "> d +"
      },
      {
        name: "decrement tape singleton",
        input: "> x [ - ]",
        output: "> \\0 [ - ]"
      },
      {
        name: "left square bracket when read char is nonzero",
        input: "\\1 [ - ]",
        output: "\\0 [ - ]"
      },
      {
        name: "left square bracket when read char is zero",
        input: "\\0 [ + ]",
        output: "\\0 [ + ]"
      },
      {
        name: "wrap around",
        input: "\\0 x > { . \\0",
        output: "\\0 x > { . x"
      },
      {
        name: "fig 4 replicator",
        input: "[ [ { . > ] - ] i i i i i i i i i i ] - ] > . { [ [ \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0 \\0",
        output: "[ [ { . > ] - ] i i i i i i i i i i ] - ] > . { [ [ [ [ { . > ] - ] i i i i i i i i i i ] - ] > . { [ ["
      }, 
      {
        name: "don't get stuck in infinite loop",
        input: "[ ]",
        output: "[ ]"
      },
      {
        name: "increment ascii wraps around",
        input: "~ [ + ]",
        output: "\\0 [ + ]"
      },
      {
        name: "decrement ascii wraps around",
        input: "\\0 -",
        output: "\\127 -"
      },
      {
        name: "close square bracket jumps properly",
        input: "\\1 [ { . - ] \\0",
        output: "\\0 [ { . - ] \\1"
      },
      {
        name: "print H",
        input: "{ \\0 \\0 > > + + + + + + + + + [ < + + + + + + + + > - ] < . \\0",
        output: "{ H \\0 > > + + + + + + + + + [ < + + + + + + + + > - ] < . H"
      },
      {
        name: "nested loop 1",
        input: "\\4 \\4 \\4 \\0 [ [ - ] > ] \\1",
        output: "\\0 \\0 \\0 \\0 [ [ - ] > ] \\1"
      },
      {
        name: "max steps",
        input: "{ [ ] . \\0",
        output: "{ [ ] . \\0"
      }
    ] \t -> do
      it ("parse " <> t.name) do
        let
          inputTape = parseBrainFuckProgram t.input
          computedInput = inputTape >>= unParseBrainFuckProgram

          outputTape = parseBrainFuckProgram t.output
          computedOutput = outputTape >>= unParseBrainFuckProgram

        (Just t.input) `shouldEqual` computedInput
        (Just t.output) `shouldEqual` computedOutput
      
      it ("execute " <> t.name) do
        let
          output = do
            inputTape <- parseBrainFuckProgram t.input
            result <- executeBrainFuckProgram $ getInitialBrainFuckExecutionContext inputTape
            unParseBrainFuckProgram result.tape

        (Just t.output) `shouldEqual` output
