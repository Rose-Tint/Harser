module Main where

import System.IO (hSetEcho, stdin)

import Examples.CalcInterpreter ()
import Examples.SimpleGrammar (mainLoop)


main :: IO ()
main = do
    _ <- hSetEcho stdin False
    mainLoop
