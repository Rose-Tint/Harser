module Char where

import Data.Char

import Parser


oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)


noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (\c -> not (elem c cs))


anyChar :: Parser Char
anyChar = satisfy (\_ -> True)


space :: Parser Char
space = satisfy isSpace


newline :: Parser Char
newline = satisfy (== '\n')


symbol :: Parser Char
symbol = satisfy isSymbol


letter :: Parser Char
letter = satisfy isAlpha


numeric :: Parser Char
numeric = satisfy isNumber


alnum :: Parser Char
alnum = satisfy isAlphaNum


floatChar :: Parser Char
floatChar = oneOf "0123456789."


digit :: Parser Char
digit = oneOf "0123456789"


hexChar :: Parser Char
hexChar = oneOf "0123456789abcdef"


line :: Parser String
line = zeroOrMore $ satisfy (/= '\n')


skipws :: Parser String
skipws = zeroOrMore space


spaces :: Parser String
spaces = oneOrMore space


word :: Parser String
word = oneOrMore letter


number :: Parser String
number = oneOrMore digit


hexNumber :: Parser String
hexNumber = oneOrMore hexChar


float :: Parser String
float = oneOrMore floatChar


wrapped :: String -> Parser a -> String -> Parser a
wrapped ls p rs = do
    _ <- string ls
    x <- p
    _ <- string rs
    return x


{- %%%%% COMMON USES FOR CONVENIENCE %%%%% -}


lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    _ <- spaces
    return x


parens :: Parser a -> Parser a
parens p = do
    _ <- char '('
    x <- p
    _ <- char ')'
    return x


varName :: Parser String
varName = do
    c  <- satisfy (\c -> isAlpha c || c == '_')
    cs <- zeroOrMore $ satisfy (\c' -> isAlphaNum c' || c' == '_')
    return (c:cs)
