module Char where

import qualified Data.Text as T
import Data.Char

import Parser


oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)


noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (\c -> not (elem c cs))


-- :: Parser Char
anyChar = satisfy (\_ -> True)
space = satisfy isSpace
newline = satisfy (== '\n')
letter = satisfy isAlpha
numeric = satisfy isNumber
alnum = satisfy isAlphaNum
floatChar = oneOf "0123456789."
digit = oneOf "0123456789"
hexChar = oneOf "0123456789abcdef"


-- Parser String
word = oneOrMore letter
number = oneOrMore digit
hexNumber = oneOrMore hexChar
float = oneOrMore floatChar


{- %%%%% COMMON USES FOR CONVENIENCE %%%%% -}


parens :: Parser a -> Parser a
parens p = inner <$> char '(' <*> p <*> char ')'
    where inner _ a _ = a


varName :: Parser String
varName = Parser (\s -> case (runParser (zeroOrOne varChar) s) of
        (s', Success Nothing)  -> runParser (oneOrMore alnum) s'
        (s', Success (Just c)) ->
            let (s'', (Success cs)) = runParser (zeroOrMore varChar) s'
            in (s'', return (c:cs)))
                where varChar = satisfy (\c -> isAlphaNum c || c == '_')
