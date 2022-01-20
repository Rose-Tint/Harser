{-# LANGUAGE FlexibleContexts #-}

module Examples.Calculator where

import System.IO (hFlush, hSetEcho, stdout, stdin)
import System.Exit (exitSuccess)

import Harser.Char
import Harser.Combinators
import Harser.Parser


data Expr
    = Num Integer
    | Add [Expr]
    | Sub [Expr]
    | Mul [Expr]
    | Parens Expr


eval :: Expr -> Integer
eval (Num n) = n
eval (Parens e) = eval e
eval (Add l) = foldl (+) 0 (fmap eval l)
eval (Sub l) = foldl (-) 0 (fmap eval l)
eval (Mul l) = foldl (*) 1 (fmap eval l)


expr :: Parser String () Expr
expr = oper <?> term


term :: Parser String () Expr
term = try num <?> parens


num :: Parser String () Expr
num = (fmap (Num . read) (oneOrMore digit))


oper :: Parser String () Expr
oper = do
    x <- term
    _ <- skipws
    op <- oneOf "+-*"
    _ <- skipws
    y <- oper <?> term
    return $ case op of
        '+' -> Add [x, y]
        '-' -> Sub [x, y]
        '*' -> Mul [x, y]
        c   -> error ("somehow did not match: " ++ [c])


parens :: Parser String () Expr
parens = do
    _ <- char '('
    e <- wrap skipws expr 
    _ <- char ')'
    return e


main :: IO ()
main = hSetEcho stdin False >> loop
    where
        loop = do
            txt <- putStr "~>>" >> hFlush stdout >> getLine
            if txt == "exit" then
                exitSuccess
            else
                case fmap eval (parse expr txt ()) of
                    (Failure e) -> putStrLn $ "!>> " ++ e
                    (Success e) -> putStrLn $ "=>> " ++ show e
            loop
