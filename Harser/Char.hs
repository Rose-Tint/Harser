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
import Harser.Parser (Parser(..), fulfill, (!>))
import Harser.Stream (Stream(..))


char :: (Stream s Char) => Char -> Parser s u Char
char c = fulfill (== c) !> "char"


string :: (Stream s Char) => String -> Parser s u String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs !> "string"


oneOf :: (Stream s Char) => [Char] -> Parser s u Char
oneOf cs = fulfill (`elem` cs) !> "oneOf"


noneOf :: (Stream s Char) => [Char] -> Parser s u Char
noneOf cs = fulfill (not . (`elem` cs)) !> "noneOf"


anyChar :: (Stream s Char) => Parser s u Char
anyChar = fulfill (\_ -> True)


space :: (Stream s Char) => Parser s u Char
space = fulfill isSpace !> "space"


newline :: (Stream s Char) => Parser s u Char
newline = fulfill (== '\n') !> "newline"


symbol :: (Stream s Char) => Parser s u Char
symbol = fulfill isSymbol !> "symbol"


letter :: (Stream s Char) => Parser s u Char
letter = fulfill isAlpha !> "letter"


alnum :: (Stream s Char) => Parser s u Char
alnum = fulfill isAlphaNum !> "alnum"


oct :: (Stream s Char) => Parser s u Char
oct = oneOf "01234567" !> "oct"


digit :: (Stream s Char) => Parser s u Char
digit = oneOf "0123456789" !> "digit"


hex :: (Stream s Char) => Parser s u Char
hex = oneOf "0123456789abcdef" !> "hex"


skipws :: (Stream s Char) => Parser s u ()
skipws = skips space


spaces :: (Stream s Char) => Parser s u ()
spaces = skips' space !> "spaces"
