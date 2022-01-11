module Main where

import Prelude hiding (getLine)

import Control.Applicative
import qualified Data.Text
import Data.Text.IO (getLine)
import System.IO hiding (getLine)
import System.Exit
import Text.Printf

import Harser.Char
import Harser.Combinators
import Harser.Parser


{- EXAMPLE: SIMPLE CALCULATOR -}


data Expr
    = Num Integer
    | Add [Expr]
    | Sub [Expr]
    | Mul [Expr]
    | Div [Expr]
    | Parens Expr


input :: IO Data.Text.Text
input = putStr "~>>" >> hFlush stdout >> getLine


output :: StreamState Expr -> IO ()
output st = case st of
    Failure e -> printf "!>> %s\n" e
    Success e -> printf "=>> %s\n" (show $ eval e)


eval :: Expr -> Integer
eval (Num n) = n
eval (Parens e) = eval e
eval (Add (x:xs)) = foldr (+) (eval x) (fmap eval xs)
eval (Sub (x:xs)) = foldr (-) (eval x) (fmap eval xs)
eval (Mul (x:xs)) = foldr (*) (eval x) (fmap eval xs)
eval (Div (x:xs)) = foldr (div) (eval x) (fmap eval xs)
eval _ = error "no arguments"


expr :: Parser Expr
expr = try oper <|> try func <|> term


term :: Parser Expr
term = try num <|> parens expr


num :: Parser Expr
num = (fmap (Num . read) number) !> "NaN"


oper :: Parser Expr
oper = do
    x <- term
    _ <- skipws
    op <- oneOf "+-*/"
    _ <- skipws
    y <- term
    return $ case op of
        '+' -> Add [x, y]
        '-' -> Sub [x, y]
        '*' -> Mul [x, y]
        '/' -> Div [x, y]
        c   -> error ("somehow did not match: " ++ [c])


func :: Parser Expr
func = do
    fn <- choose (fmap string ["add", "sub", "mul", "div"])
    _ <- spaces
    args <- sepBy spaces term
    return $ case fn of
        "add" -> Add args
        "sub" -> Sub args
        "mul" -> Mul args
        "div" -> Div args
        s     -> error ("somehow did not match: " ++ s)


main :: IO ()
main = do
    _ <- hSetEcho stdin False
    loop


loop :: IO ()
loop = do
    s <- input
    if s == Data.Text.pack "exit" then
        exitSuccess
    else
        output (parse expr s)
    loop