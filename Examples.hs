module Main where

import qualified Examples.CalcWithVars as E1 (run)
import qualified Examples.Calculator as E2 (run)


main :: IO ()
main = do
    putStrLn $ replicate 9 '~'
    putStr "CalcWithVars"
    putStrLn $ replicate 9 '~'
    E1.run
    putStrLn $ replicate 10 '~'
    putStr "Calculator"
    putStrLn $ replicate 10 '~'
    E2.run