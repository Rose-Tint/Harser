module Main where

import Text.Printf
import System.Exit
import System.IO
import Control.Applicative

import Parser
import Char


data Expr
    = Value Integer
    | Func ([Expr] -> Integer) [Expr]
    | Parens Expr


input :: IO String
input = putStr "~>>" >> hFlush stdout >> getLine


eval :: Expr -> Integer
eval (Num n) = n
eval (Func f l) = f l
eval (Parens e) = eval e


expr :: Parser Expr
expr = try add <|> try num <|> parens expr


term :: Parser Expr
term = try num <|> parens expr


num :: Parser Expr
num = (fmap (Num . read) number) !> "NaN"


addOp :: Parser Expr
addOp = do
    args <- sepBy (wrapped skipws (char '+')) term
    return (Func (sum . fmap eval) args) !> "not add"


addFn :: Parser Expr
addFn = do
    _ <- lexeme $ string "add"
    args <- sepBy spaces term
    return (Func (sum . fmap eval) args) !> "not (+)"


add :: Parser Expr
add = try addOp <|> addFn


main :: IO ()
main = do
    _ <- hSetEcho stdin False
    loop


loop :: IO ()
loop = do
    s <- input
    if s == "exit" then
        exitSuccess
    else
        case parseStr expr s of
            Failure e -> printf "!>> %s\n" e
            Success e -> printf "=>> %d\n" (eval e)
    loop
