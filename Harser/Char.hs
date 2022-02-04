{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Harser.Char
Description : Character parsing tools

This module contains many useful and common character
parsers,
-}

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


-- |@'char' ch@ parses and returns one instance of @ch@
char :: (Stream s Char) => Char -> Parser s u Char
char c = satisfy (== c) !> "char"


-- |@'string' str@ parses and returns one instance of
-- @str@
string :: (Stream s Char) => String -> Parser s u String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs !> "string"


-- |@'oneOf' cs@ parses any one of @cs@ and returns
-- the parsed character.
oneOf :: (Stream s Char) => [Char] -> Parser s u Char
oneOf cs = satisfy (`elem` cs) !> "oneOf"


-- |@'noneOf' cs@ parses and returns any character that
-- is not one of @cs@.
noneOf :: (Stream s Char) => [Char] -> Parser s u Char
noneOf cs = satisfy (not . (`elem` cs)) !> "noneOf"


-- |Parses any character and returns the result.
anyChar :: (Stream s Char) => Parser s u Char
anyChar = satisfy (\_ -> True)


-- |Parses a whitespace character (a character that
-- satisfies @'isSpace'@).
space :: (Stream s Char) => Parser s u Char
space = satisfy isSpace !> "space"


-- |Parses a newline ('\\n') character.
newline :: (Stream s Char) => Parser s u Char
newline = satisfy (== '\n') !> "newline"


-- |Parses a symbol (a character that
-- satisfies @'isSymbol'@).
symbol :: (Stream s Char) => Parser s u Char
symbol = satisfy isSymbol !> "symbol"


-- |Parses a letter (a character that satisfies
-- @'isAlpha'@
letter :: (Stream s Char) => Parser s u Char
letter = satisfy isAlpha !> "letter"


-- |Parses a letter or number (a character that
-- satisfies @'isAlphaNum'@
alnum :: (Stream s Char) => Parser s u Char
alnum = satisfy isAlphaNum !> "alnum"


-- |Parses an octal digit (0 through 7)
oct :: (Stream s Char) => Parser s u Char
oct = oneOf "01234567" !> "oct"


-- |Parses a decimal digit (0 through 9)
digit :: (Stream s Char) => Parser s u Char
digit = oneOf "0123456789" !> "digit"


-- |Parses a hexadecimal digit (0 through 9 or 'a'
-- through 'f')
hex :: (Stream s Char) => Parser s u Char
hex = oneOf "0123456789abcdef" !> "hex"


-- |Skips zero or more whitespace characters
skipws :: (Stream s Char) => Parser s u ()
skipws = skips space


-- |Skips one or more whitespace characters
spaces :: (Stream s Char) => Parser s u ()
spaces = skips' space !> "spaces"
