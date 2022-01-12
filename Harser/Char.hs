{-# LANGUAGE FlexibleContexts #-}

module Harser.Char where

import Data.Char

import Harser.Combinators
import Harser.Parser
import Harser.Stream


char :: (Stream s Char) => Char -> Parser s Char
char c = satisfy (== c)


string :: (Stream s Char) => String -> Parser s String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs


oneOf :: (Stream s Char) => [Char] -> Parser s Char
oneOf cs = satisfy (`elem` cs)


noneOf :: (Stream s Char) => [Char] -> Parser s Char
noneOf cs = satisfy (\c -> not (elem c cs))


anyChar :: (Stream s Char) => Parser s Char
anyChar = satisfy (\_ -> True)


space :: (Stream s Char) => Parser s Char
space = satisfy isSpace


newline :: (Stream s Char) => Parser s Char
newline = satisfy (== '\n')


symbol :: (Stream s Char) => Parser s Char
symbol = satisfy isSymbol


letter :: (Stream s Char) => Parser s Char
letter = satisfy isAlpha


numeric :: (Stream s Char) => Parser s Char
numeric = satisfy isNumber


alnum :: (Stream s Char) => Parser s Char
alnum = satisfy isAlphaNum


floatChar :: (Stream s Char) => Parser s Char
floatChar = oneOf "0123456789."


digit :: (Stream s Char) => Parser s Char
digit = oneOf "0123456789"


hexChar :: (Stream s Char) => Parser s Char
hexChar = oneOf "0123456789abcdef"


line :: (Stream s Char) => Parser s String
line = zeroOrMore $ satisfy (/= '\n')


skipws :: (Stream s Char) => Parser s String
skipws = zeroOrMore space


spaces :: (Stream s Char) => Parser s String
spaces = oneOrMore space


word :: (Stream s Char) => Parser s String
word = oneOrMore letter


number :: (Stream s Char) => Parser s String
number = oneOrMore digit


hexNumber :: (Stream s Char) => Parser s String
hexNumber = oneOrMore hexChar


float :: (Stream s Char) => Parser s String
float = oneOrMore floatChar


{- %%%%% COMMON USES FOR CONVENIENCE %%%%% -}


lexeme :: (Stream s Char) => Parser s a -> Parser s a
lexeme p = do
    x <- p
    _ <- spaces
    return x


parens :: (Stream s Char) => Parser s a -> Parser s a
parens p = do
    _ <- char '('
    _ <- skipws
    x <- p
    _ <- skipws
    _ <- char ')'
    return x
