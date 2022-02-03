module Main where

import Text.Printf (printf)
import System.Exit (exitSuccess)

import qualified Examples.CalcWithVars (run)
import qualified Examples.Calculator (run)
import qualified Examples.Lang.Lang (run)
import qualified Examples.Json (run)


runExample :: String -> IO () -> IO ()
runExample nm r = (putStrLn $ take 30 str) >> r
    where
        del = replicate ((28 - length nm) `div` 2) '~'
        str = printf "%s %s %s" del nm del


main :: IO ()
main = do
    _ <- runExample "Json"          Examples.Json.run
    _ <- runExample "Lang"          Examples.Lang.Lang.run
    _ <- runExample "CalcWithVars"  Examples.CalcWithVars.run
    _ <- runExample "Calculator"    Examples.Calculator.run
    exitSuccess