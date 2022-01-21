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
char c = satisfy (== c) !> ['\'',c,'\'']


string :: (Stream s Char) => String -> Parser s u String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs


oneOf :: (Stream s Char) => [Char] -> Parser s u Char
oneOf cs = satisfy (`elem` cs) !> (" | not one of " ++ cs)


noneOf :: (Stream s Char) => [Char] -> Parser s u Char
noneOf cs = satisfy (not . (`elem` cs)) !> ("| is one of " ++ cs)


anyChar :: (Stream s Char) => Parser s u Char
anyChar = satisfy (\_ -> True)


space :: (Stream s Char) => Parser s u Char
space = satisfy isSpace !> " [isSpace]"


newline :: (Stream s Char) => Parser s u Char
newline = satisfy (== '\n')


symbol :: (Stream s Char) => Parser s u Char
symbol = satisfy isSymbol !> " [isSymbol]"


letter :: (Stream s Char) => Parser s u Char
letter = satisfy isAlpha !> " [isAlpha]"


alnum :: (Stream s Char) => Parser s u Char
alnum = satisfy isAlphaNum !> " [isAlphaNum]"


oct :: (Stream s Char) => Parser s u Char
oct = oneOf "01234567" !> "was  [digit]"


digit :: (Stream s Char) => Parser s u Char
digit = oneOf "0123456789" !> " | not a digit"


hex :: (Stream s Char) => Parser s u Char
hex = oneOf "0123456789abcdef" !> " | not a hex digit"


skipws :: (Stream s Char) => Parser s u ()
skipws = skips space


spaces :: (Stream s Char) => Parser s u ()
spaces = skips' space
