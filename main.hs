module Main where

import Prelude hiding (getLine)

import Data.Text (Text, pack)
import Data.Text.IO (getLine)
import System.IO hiding (getLine)
import System.Exit

import Harser.Parser
import Examples.Calculator (run)


input :: IO Text
input = putStr "~>>" >> hFlush stdout >> getLine


output :: (Show a) => ParseState a -> IO ()
output (Failure e) = putStrLn $ "!>> " ++ e
output (Success e) = putStrLn $ "=>> " ++ show e


loop :: IO ()
loop = do
    txt <- input
    if txt == pack "exit" then
        exitSuccess
    else
        output (run txt)
    loop


main :: IO ()
main = do
    _ <- hSetEcho stdin False
    loop
