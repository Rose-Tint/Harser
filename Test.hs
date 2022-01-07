module Tests where

import Parser
import Char


test :: (Eq a) => String -> a -> a -> IO ()
test name giv f e = putStrLn $ name ++ res
    where res = bool ": Fail" ": Pass" (f giv e)


testZeroOrOne :: Parser Char -> Maybe Char -> String -> IO ()
testZeroOrOne p e s = test "zeroOrOne" res e
    where res = parseStr (zeroOrOne p) s


testZeroOrMore :: Parser Char -> String -> String -> IO ()
testZeroOrMore p e s = test "zeroOrMore" res e
    where res = parseStr (zeroOrMore p) s


testString :: Parser String -> String -> String -> IO ()
testString p e s = test "string" res e
    where res = parseStr (string p) s
