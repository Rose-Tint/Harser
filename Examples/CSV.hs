module Examples.CSV where

import Prelude hiding (readFile)

import Data.Text (Text)
import Data.Text.IO (readFile)
import System.IO hiding (readFile)

import Harser.Char
import Harser.Combinators
import Harser.Parser


csv :: Parser Text () [[String]]
csv = zeroOrMore line


line :: Parser Text () [String]
line = zeroOrMore cell


cell :: Parser Text () String
cell = wrap skipws (zeroOrMore (noneOf ",\n") <* oneOf ",\n")


main :: IO ()
main = do
    putStr "Filename? ";
    hFlush stdout;
    fname <- getLine
    contents <- readFile fname
    print $ parse csv contents ()
