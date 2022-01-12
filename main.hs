module Main where

import System.IO (hSetEcho, stdin)

import Examples.CalcInterpreter (mainLoop)
import Examples.SimpleGrammar ()


main :: IO ()
main = do
    hSetEcho stdin False
    mainLoop
