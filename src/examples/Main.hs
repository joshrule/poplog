module Main where

-- internal imports
import Poplog.Parser
import Poplog.Evaluator
import Repl

-- Main
main :: IO ()
main = conditionalRepl parseProlog eval "Prolog"
