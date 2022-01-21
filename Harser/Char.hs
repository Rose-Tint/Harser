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
    skipws,
    spaces,
) where

import Data.Char (isSpace, isSymbol, isAlpha, isAlphaNum)

import Harser.Combinators (skips, skips')
import Harser.Parser (Parser(..), satisfy, (!>))
import Harser.Stream (Stream(..))


char :: (Stream s Char) => Char -> Parser s u Char
char c = satisfy (== c) !> " | char " ++ [c]


string :: (Stream s Char) => String -> Parser s u String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs !> " | string"


oneOf :: (Stream s Char) => [Char] -> Parser s u Char
oneOf cs = satisfy (`elem` cs) !> (" | oneOf " ++ cs)


noneOf :: (Stream s Char) => [Char] -> Parser s u Char
noneOf cs = satisfy (not . (`elem` cs))
    !> (" | noneOf " ++ cs)


anyChar :: (Stream s Char) => Parser s u Char
anyChar = satisfy (\_ -> True)


space :: (Stream s Char) => Parser s u Char
space = satisfy isSpace !> " | space"


newline :: (Stream s Char) => Parser s u Char
newline = satisfy (== '\n') !> " | newline"


symbol :: (Stream s Char) => Parser s u Char
symbol = satisfy isSymbol !> " | symbol"


letter :: (Stream s Char) => Parser s u Char
letter = satisfy isAlpha !> " | letter"


alnum :: (Stream s Char) => Parser s u Char
alnum = satisfy isAlphaNum !> " | alnum"


oct :: (Stream s Char) => Parser s u Char
oct = oneOf "01234567" !> " | oct"


digit :: (Stream s Char) => Parser s u Char
digit = oneOf "0123456789" !> " | digit"


hex :: (Stream s Char) => Parser s u Char
hex = oneOf "0123456789abcdef" !> " | hex"


skipws :: (Stream s Char) => Parser s u ()
skipws = skips space


spaces :: (Stream s Char) => Parser s u ()
spaces = skips' space
