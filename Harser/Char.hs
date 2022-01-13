{-# LANGUAGE FlexibleContexts #-}

module Harser.Char where

import Data.Char

import Harser.Combinators
import Harser.Parser
import Harser.Stream


char :: (Stream s Char) => Char -> Parser s u Char
char c = satisfy (== c)


string :: (Stream s Char) => String -> Parser s u String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs


oneOf :: (Stream s Char) => [Char] -> Parser s u Char
oneOf cs = satisfy (`elem` cs)


noneOf :: (Stream s Char) => [Char] -> Parser s u Char
noneOf cs = satisfy (\c -> not (elem c cs))


anyChar :: (Stream s Char) => Parser s u Char
anyChar = satisfy (\_ -> True)


space :: (Stream s Char) => Parser s u Char
space = satisfy isSpace


newline :: (Stream s Char) => Parser s u Char
newline = satisfy (== '\n')


symbol :: (Stream s Char) => Parser s u Char
symbol = satisfy isSymbol


letter :: (Stream s Char) => Parser s u Char
letter = satisfy isAlpha


numeric :: (Stream s Char) => Parser s u Char
numeric = satisfy isNumber


alnum :: (Stream s Char) => Parser s u Char
alnum = satisfy isAlphaNum


floatChar :: (Stream s Char) => Parser s u Char
floatChar = oneOf "0123456789."


digit :: (Stream s Char) => Parser s u Char
digit = oneOf "0123456789"


hexChar :: (Stream s Char) => Parser s u Char
hexChar = oneOf "0123456789abcdef"


line :: (Stream s Char) => Parser s u String
line = zeroOrMore $ satisfy (/= '\n')


skipws :: (Stream s Char) => Parser s u String
skipws = zeroOrMore space


spaces :: (Stream s Char) => Parser s u String
spaces = oneOrMore space


word :: (Stream s Char) => Parser s u String
word = oneOrMore letter


number :: (Stream s Char) => Parser s u String
number = oneOrMore digit


hexNumber :: (Stream s Char) => Parser s u String
hexNumber = oneOrMore hexChar


float :: (Stream s Char) => Parser s u String
float = oneOrMore floatChar


{- %%%%% COMMON USES FOR CONVENIENCE %%%%% -}


lexeme :: (Stream s Char) => Parser s u a -> Parser s u a
lexeme p = do
    x <- p
    _ <- spaces
    return x


parens :: (Stream s Char) => Parser s u a -> Parser s u a
parens p = do
    _ <- char '('
    _ <- skipws
    x <- p
    _ <- skipws
    _ <- char ')'
    return x
