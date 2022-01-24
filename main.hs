module Main where

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Stream ()
import Harser.Testing
import Harser.Utilities

import qualified Examples.Calculator as E1 (run)
import qualified Examples.CalcWithVars as E2 (run)


main :: IO ()
main = do
    tests
    putStrLn $ replicate 30 '~'
    E1.run
    putStrLn $ replicate 30 '~'
    E2.run
    putStrLn $ replicate 30 '~'
    E3.run


printTest :: String -> Bool -> IO ()
printTest s b = if b then
        putStrLn $ ('\t':s) ++ "\027[32mPass\027[0m"
    else
        putStrLn $ ('\t':s) ++ "\027[31mFail\027[0m"


utilitiesTests :: IO ()
utilitiesTests = do
    printTest "fractional ...... " (parserTest test_fractional)
    printTest "integral ........ " (parserTest test_integral)
        where
            test_fractional = statelessTest {
                parser = fractional, 
                stream = "123.456",
                expResult = 123.456
            }
            test_integral = statelessTest {
                parser = integral,
                stream = "123456",
                expResult = 123456
            }
            

combinatorsTests :: IO ()
combinatorsTests = do
    printTest "zeroOrOne ....... " (parserTest test1_zeroOrOne)
    printTest "zeroOrOne ....... " (parserTest test2_zeroOrOne)
    printTest "oneOrMore ....... " (parserTest test_oneOrMore)
    printTest "count ........... " (parserTest test_count)
    printTest "atLeast ......... " (parserTest test_atLeast)
    printTest "atMost .......... " (parserTest test_atMost)
    printTest "sepBy ........... " (parserTest test_sepBy)
    printTest "wrap ............ " (parserTest test_wrap)
    printTest "between ......... " (parserTest test_between)
        where
            test1_zeroOrOne = statelessTest {
                parser = zeroOrOne (string "foo"),
                stream = "Hello!",
                expResult = Nothing
            }
            test2_zeroOrOne = statelessTest {
                parser = zeroOrOne (string "Hel"),
                stream = "Hello!",
                expResult = Just "Hel"
            }
            test_oneOrMore = statelessTest {
                parser = oneOrMore letter,
                stream = "abc123",
                expResult = "abc"
            }
            test_count = statelessTest {
                parser = count 3 (char 'a'),
                stream = "aaaabc",
                expResult = "aaa"
            }
            test_atLeast = statelessTest {
                parser = atLeast 3 (char 'a'),
                stream = "aaaabc",
                expResult = "aaaa"
            }
            test_atMost = statelessTest {
                parser = atMost 5 (char 'a'),
                stream = "aaabc",
                expResult = "aaa"
            }
            test_sepBy = statelessTest {
                parser = sepBy (char ';') anyChar,
                stream = "a;b;c",
                expResult = "abc"
            }
            test_wrap = statelessTest {
                parser = wrap (char ' ') (oneOrMore alnum),
                stream = " abc 123",
                expResult = "abc"
            }
            test_between = statelessTest {
                parser = between (char '(') alnum (char ')'),
                stream = "(a)",
                expResult = 'a'
            }


parserTests :: IO ()
parserTests = do
    printTest "getStream ....... " (parserTest test_getStream)
    printTest "getState ........ " (parserTest test_getState)
    printTest "putState ........ " (parserTest test_putState)
    printTest "modifyState ..... " (parserTest test_modifyState)
    printTest "stateChanges .... " (parserTest test_stateChanges)
    printTest "satisfy ......... " (parserTest test_satisfy)
        where
            test_getStream = statelessTest {
                parser = getStream,
                stream = "Hello!",
                expResult = "Hello!"
            }
            test_getState = ParserTest {
                parser = getState, stream = " ",
                initState = [1, 2],
                expState = [1, 2],
                expResult = [1, 2]
            }
            test_putState = ParserTest {
                parser = putState [1, 2],
                stream = " ",
                initState = [],
                expState = [1, 2],
                expResult = ()
            }
            test_modifyState = ParserTest {
                parser = modifyState (+1),
                stream = " ",
                initState = 2,
                expState = 3,
                expResult = ()
            }
            test_stateChanges = ParserTest {
                parser = do
                    _ <- putState ['c', 'd', 'e']
                    b <- char 'b'
                    _ <- modifyState (b:)
                    a <- char 'a'
                    s <- getState
                    putState (a:s)
                    return b,
                stream = "ba",
                initState = "hi",
                expState = "abcde",
                expResult = 'b'
            }
            test_satisfy = statelessTest {
                parser = char 'H',
                stream = "Hello!",
                expResult = 'H'
            }


tests :: IO ()
tests = putStrLn "Harser.Parser"      >> parserTests
    >>  putStrLn "Harser.Combinators" >> combinatorsTests
    >>  putStrLn "Harser.Utilities"   >> utilitiesTests
