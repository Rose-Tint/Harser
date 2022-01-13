{-# LANGUAGE FlexibleContexts #-}

module Examples.Calculator where

import Prelude hiding (getLine)

import Control.Applicative ()
import Data.Text (Text)

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Stream


data Expr
    = Num Integer
    | Add [Expr]
    | Sub [Expr]
    | Mul [Expr]
    | Div [Expr]
    | Parens Expr


run :: Text -> ParseState Integer
run t = fmap eval (parse expr t ())


eval :: Expr -> Integer
eval (Num n) = n
eval (Parens e) = eval e
eval (Add (x:xs)) = foldl (+) (eval x) (fmap eval xs)
eval (Sub (x:xs)) = foldl (-) (eval x) (fmap eval xs)
eval (Mul (x:xs)) = foldl (*) (eval x) (fmap eval xs)
eval (Div (x:xs)) = foldl (div) (eval x) (fmap eval xs)
eval _ = error "no arguments"


expr :: (Stream s Char) => Parser s () Expr
expr = oper <?> func <?> term


term :: (Stream s Char) => Parser s () Expr
term = try num <?> parens


num :: (Stream s Char) => Parser s () Expr
num = (fmap (Num . read) (oneOrMore digit)) !> "NaN"


oper :: (Stream s Char) => Parser s () Expr
oper = do
    x <- term
    _ <- skipsp
    op <- oneOf "+-*/"
    _ <- skipsp
    y <- oper <?> term
    return $ case op of
        '+' -> Add [x, y]
        '-' -> Sub [x, y]
        '*' -> Mul [x, y]
        '/' -> Div [x, y]
        c   -> error ("somehow did not match: " ++ [c])


func :: (Stream s Char) => Parser s () Expr
func = do
    fn <- select' string ["add", "sub", "mul", "div"]
    _ <- spaces
    args <- sepBy' spaces term
    return $ case fn of
        "add" -> Add args
        "sub" -> Sub args
        "mul" -> Mul args
        "div" -> Div args
        s     -> error ("somehow did not match: " ++ s)


parens :: (Stream s Char) => Parser s () Expr
parens = do
    _ <- char '('
    e <- wrap skipsp expr 
    _ <- char ')'
    return e
