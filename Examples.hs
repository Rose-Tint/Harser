module Main where

import System.Exit (exitSuccess)

import qualified Examples.CalcWithVars as E1 (run)
import qualified Examples.Calculator as E2 (run)
import Examples.Lang ()


main :: IO ()
main = do
    putStr $ replicate 8 '~'
    putStr " CalcWithVars "
    putStrLn $ replicate 8 '~'
    E1.run
    putStr $ replicate 9 '~'
    putStr " Calculator "
    putStrLn $ replicate 9 '~'
    E2.run
    putStrLn $ replicate 30 '~'
    exitSuccess