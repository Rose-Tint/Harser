module Main where

import System.Exit

import Harser.Char
import Harser.Combinators
import Harser.Testing

import Tests.TestTools
            

main :: IO ()
main = do
    _ <- putStrLn "Harser.Combinators"
    t1 <- doTest "zeroOrOne" test1_zeroOrOne
    t2 <- doTest "zeroOrOne" test2_zeroOrOne
    t3 <- doTest "oneOrMore" test_oneOrMore
    t4 <- doTest "count" test_count
    t5 <- doTest "atLeast" test_atLeast
    t6 <- doTest "atMost" test_atMost
    t7 <- doTest "splits" test_splits
    t8 <- doTest "wrap" test_wrap
    t9 <- doTest "between" test_between
    if and [t1,t2,t3,t4,t5,t6,t7,t8,t9] then
        exitSuccess
    else
        exitFailure
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
                parser = between (char 'a') alnum (char 'c'),
                stream = "abc",
                expResult = 'b'
            }
