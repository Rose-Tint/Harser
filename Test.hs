module Main where

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Stream ()
import Harser.Testing
import Harser.Utilities


main :: IO ()
main = do
    putStrLn $ replicate 30 '~'
    tests
    putStrLn $ replicate 30 '~'


printTest :: (Eq u, Eq a) =>
    String -> ParserTest s u a -> IO ()
printTest s b = putStrLn $ ('\t':s) ++ (
    if parserTest b then
        "\027[32mPass\027[0m"
    else
        "\027[31mFail\027[0m")


utilitiesTests :: IO ()
utilitiesTests = do
    printTest "fractional ...... " test_fractional
    printTest "integral ........ " test_integral
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
    printTest "zeroOrOne ....... " test1_zeroOrOne
    printTest "zeroOrOne ....... " test2_zeroOrOne
    printTest "oneOrMore ....... " test_oneOrMore
    printTest "count ........... " test_count
    printTest "atLeast ......... " test_atLeast
    printTest "atMost .......... " test_atMost
    printTest "splits .......... " test_splits
    printTest "wrap ............ " test_wrap
    printTest "between ......... " test_between
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
            test_splits = statelessTest {
                parser = char ';' `splits` anyChar,
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
    printTest "getStream ....... " test_getStream
    printTest "getState ........ " test_getState
    printTest "putState ........ " test_putState
    printTest "modifyState ..... " test_modifyState
    printTest "stateChanges .... " test_stateChanges
    printTest "satisfy ......... " test_satisfy
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
                parser = setState [1, 2],
                stream = " ",
                initState = [],
                expState = [1, 2],
                expResult = ()
            }
            test_modifyState = ParserTest {
                parser = amendState (+1),
                stream = " ",
                initState = 2,
                expState = 3,
                expResult = ()
            }
            test_stateChanges = ParserTest {
                parser = do
                    _ <- setState ['c', 'd', 'e']
                    b <- char 'b'
                    _ <- amendState (b:)
                    a <- char 'a'
                    s <- getState
                    setState (a:s)
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
