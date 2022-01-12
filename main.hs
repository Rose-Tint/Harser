module Main where

import Harser.Char ()
import Harser.Combinators ()
import Harser.Parser ()
import Harser.Stream ()

import System.IO (hSetEcho, stdin)

-- import Examples.CalcInterpreter ()
-- import Examples.SimpleGrammar (mainLoop)


main :: IO ()
main = do
    hSetEcho stdin False
    -- mainLoop
