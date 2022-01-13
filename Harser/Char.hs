{-# LANGUAGE FlexibleContexts #-}

module Harser.Char (
    char,
    string,
    oneOf,
    noneOf,
    anyChar,
    space,
    newline,
    symbol,
    letter,
    alnum,
    oct,
    digit,
    hex,
    skipsp,
    spaces,
    float,
) where

import Data.Char (isSpace, isSymbol, isAlpha, isAlphaNum)

import Harser.Combinators (oneOrMore, skips, skips')
import Harser.Parser (Parser(..), satisfy)
import Harser.Stream (Stream(..))


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


alnum :: (Stream s Char) => Parser s u Char
alnum = satisfy isAlphaNum


oct :: (Stream s Char) => Parser s u Char
oct = oneOf "01234567"


digit :: (Stream s Char) => Parser s u Char
digit = oneOf "0123456789"


hex :: (Stream s Char) => Parser s u Char
hex = oneOf "0123456789abcdef"


skipsp :: (Stream s Char) => Parser s u ()
skipsp = skips space


spaces :: (Stream s Char) => Parser s u ()
spaces = skips' space


float :: (Stream s Char) => Parser s u String
float = do
    whole <- oneOrMore digit
    dot <- char '.'
    dec <- oneOrMore digit
    return (whole ++ (dot:dec))
