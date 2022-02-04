module Main where

import System.Exit

import Harser.Char
import Harser.Parser
import Harser.Testing

import Tests.TestTools


main :: IO ()
main = do
    _ <- putStrLn "Harser.Parser"
    t1 <- doTest "getStream" test_getStream
    t2 <- doTest "getState" test_getState
    t3 <- doTest "setState" test_setState
    t4 <- doTest "fmapState" test_fmapState
    t5 <- doTest "stateChanges" test_stateChanges
    t6 <- doTest "satisfy" test_satisfy
    if and [t1,t2,t3,t4,t5,t6] then
        exitSuccess
    else
        exitFailure
        where
            test_getStream = statelessTest {
                parser = getStream,
                stream = "Hello!",
                expResult = "Hello!"
            }
            test_getState = ParserTest {
                parser = getState, stream = " ",
                initState = [1, 2] :: [Integer],
                expState = [1, 2] :: [Integer],
                expResult = [1, 2] :: [Integer]
            }
            test_setState = ParserTest {
                parser = setState [1, 2],
                stream = "",
                initState = [],
                expState = [1, 2] :: [Integer],
                expResult = ()
            }
            test_fmapState = ParserTest {
                parser = fmapState (+ 1),
                stream = "",
                initState = 2 :: Integer,
                expState = 3,
                expResult = ()
            }
            test_stateChanges = ParserTest {
                parser = do
                    _ <- setState ['c', 'd', 'e']
                    b <- char 'b'
                    _ <- fmapState (b:)
                    a <- char 'a'
                    s <- getState
                    _ <- setState (a:s)
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
