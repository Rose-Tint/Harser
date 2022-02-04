module Main where

import Prelude hiding (lookup, fail)

import System.IO (hSetEcho, hFlush, stdout, stdin)
import System.Exit (exitSuccess)
import Data.Map (fromList)

import Harser.Parser hiding (State)

import Examples.Lang.Data
import Examples.Lang.State
import Examples.Lang.Lang


stdlib :: State
stdlib = State {
        stack = [fromList [
            (func "add"
                [Par "a" Integer, Par "b" Integer]
                Integer
                (FuncCall "" [VarVal "a", VarVal "b"])),
            (func "int"
                [Par "a" Integer]
                Integer
                (FuncCall "" [VarVal "a"]))
        ]]
        -- types = fromList [
        --     rec "Int"
        -- ]
    }
        where
            -- rec nm = (nm, Record nm [])
            func nm ps rtn bd =
                (nm, Var nm FuncType (FuncVal $
                    Function nm ps rtn bd True))


inlnPrompt :: String -> IO String
inlnPrompt s = do
    _ <- putStr s
    _ <- hFlush stdout
    getLine


main :: IO ()
main = hSetEcho stdin False >> (loop stdlib)


loop :: State -> IO ()
loop st = inlnPrompt "~>>" >>= (\inp -> case inp of
    "exit"  -> exitSuccess
    "stop"  -> return ()
    "stack" -> do
        putStrLn $ "?>> " ++ show st
        loop st
    "show"  -> do
        nm <- inlnPrompt " >>"
        vr <- return $ parse (findVar nm) "" st
        _ <- putStrLn $ "?>> " ++ show vr
        loop st
    "clear" -> loop stdlib
    _       -> case parse'' lexer inp st of
        (st', Failure e)        -> do
            let col = getColPos st'
            _ <- putStr "*>> "
            _ <- putStr $ replicate (col - 1) '~'
            _ <- putChar '^'
            _ <- putStrLn $ replicate (length inp - col - 1) '~'
            _ <- putStrLn $ "!>> error: " ++ e
            loop $ getStateUser st'
        (st', Success e) -> do
            _ <- putStrLn $ "=>> " ++ show e
            loop (getStateUser st'))
