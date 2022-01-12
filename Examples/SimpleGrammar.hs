module Examples.SimpleGrammar where

import Prelude hiding (exp, getLine)

import Data.Text (pack)
import Data.Text.IO (getLine)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)

import Harser.Char
import Harser.Combinators
import Harser.Parser


-- https://stackoverflow.com/questions/12712149/haskell-parser-to-ast-data-type-assignment
{- GRAMMAR
    Dig  ::= 0 | .. | 9
    Int  ::= Dig | Dig Int
    Var  ::= a | .. | z | A | .. | Z
    Expr ::= Int | Expr + Expr | Expr - Expr | Expr * Expr | Var | "let" Var "=" Expr "in" Expr
-}


data AST
    = Num Integer
    | Var Char
    | Parens AST
    | Add AST AST
    | Sub AST AST
    | Mul AST AST
    | Let {
        letVar :: Char,
        letEq  :: AST,
        letIn  :: AST
    }
    

instance Show AST where
    show ast = impl 0 ast
        where
            tabs n = replicate n '\t'
            impl :: Int -> AST -> String
            impl n (Num x) = tabs n ++ "[" ++ show x ++ "]\n"
            impl n (Var c) = tabs n ++ "[" ++ [c] ++ "]\n"
            impl n (Parens a) = tabs n ++ "[()]\n" ++ impl n a
            impl n (Add x y) = impl (n + 1) x ++ tabs n ++ "[+]\n" ++ impl (n + 1) y
            impl n (Sub x y) = impl (n + 1) x ++ tabs n ++ "[-]\n" ++ impl (n + 1) y
            impl n (Mul x y) = impl (n + 1) x ++ tabs n ++ "[*]\n" ++ impl (n + 1) y
            impl n (Let c eq ex) =
                impl (n + 1) eq ++ tabs n ++ "[let " ++ [c] ++ "]\n" ++ impl (n + 1) ex
            -- impl _ _ = error "Invalid AST"


expr :: Parser AST
expr = oper <?> letE <?> term


letE :: Parser AST
letE = do
    _ <- lexeme $ string "let"
    var <- letter
    _ <- wrapped skipws (char '=')
    eq <- term
    _ <- wrapped skipws (lexeme $ string "in")
    exp <- term
    return (Let var eq exp)


add :: Parser AST
add = do
    x <- term <* skipws
    _ <- char '+'
    y <- skipws *> term
    return (Add x y)


sub :: Parser AST
sub = do
    x <- term <* skipws
    _ <- char '-'
    y <- skipws *> term
    return (Sub x y)


mul :: Parser AST
mul = do
    x <- term <* skipws
    _ <- char '*'
    y <- skipws *> term
    return (Mul x y)


term :: Parser AST
term = int <?> (fmap Var letter) <?> (fmap Parens (parens oper))


oper :: Parser AST
oper = add <?> sub <?> mul


int :: Parser AST
int = fmap (Num . read) (oneOrMore digit)


mainLoop :: IO ()
mainLoop = do
    txt <- putStr "~>>" >> hFlush stdout >> getLine
    if txt == pack "exit" then
        exitSuccess
    else case parse (sepBy (char ';') expr) txt of
        (Failure e)   -> putStrLn $ "!>> " ++ e
        (Success ast) -> putStrLn $ "=>>\n" ++ show ast
    mainLoop
