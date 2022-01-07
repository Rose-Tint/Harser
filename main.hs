module Main where

import System.IO
import System.Exit
import qualified Data.Text as T
import Debug.Trace

import Parser
import Char


prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine


parseExit :: Parser String
parseExit = try $ string "exit"


parseParens :: Parser String
parseParens = try $ parens (zeroOrMore anyChar)



main = do
    str <- prompt "Enter input:";
    case parseStr parseExit str of
        Success _ -> exitSuccess
        Failure _ -> case parseStr parseParens str of
            Success s -> putStrLn $ "removed parentheses :)\n\t" ++ show s
            Failure e -> putStrLn $ "i didn't catch that. reason: " ++ e
    main;
