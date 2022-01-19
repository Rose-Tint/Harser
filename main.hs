module Main where

import Harser.Char
import Harser.Combinators
import Harser.Lang ()
import Harser.Parser
import Harser.Stream ()
import Harser.Testing

import Examples.Calculator ()
import Examples.CSV ()


printTest :: String -> Bool -> IO ()
printTest s b = putStrLn (s ++ show b)


combinatorsTests :: IO ()
combinatorsTests = do
    printTest "zeroOrOneTest1 ... " (parserTest zeroOrOneTest1)
    printTest "zeroOrOneTest2 ... " (parserTest zeroOrOneTest2)
    printTest "oneOrMoreTest .... " (parserTest oneOrMoreTest)
    printTest "wrapTest ......... " (parserTest wrapTest)
    printTest "betweenTest ...... " (parserTest betweenTest)
        where
            zeroOrOneTest1 = statelessTest {
                parser = zeroOrOne (string "foo"),
                stream = "Hello!", expResult = Nothing
            }
            zeroOrOneTest2 = statelessTest {
                parser = zeroOrOne (string "Hel"),
                stream = "Hello!", expResult = Just "Hel"
            }
            oneOrMoreTest = statelessTest {
                parser = oneOrMore letter,
                stream = "abc123", expResult = "abc"
            }
            -- TODO: sepBy sepBy'
            -- TODO: atLeast
            -- TODO: atMost
            -- TODO: count
            -- TODO: choose choose'
            -- TODO: select select'
            wrapTest = statelessTest {
                parser = wrap (char ' ') (oneOrMore alnum),
                stream = " abc 123", expResult = "abc"
            }
            betweenTest = statelessTest {
                parser = between (char '(') alnum (char ')'),
                stream = "(a)", expResult = 'a'
            }


parserTests :: IO ()
parserTests = do
    printTest "getStream ........ " (parserTest getStreamTst)
    printTest "getState ......... " (parserTest getStateTst)
    printTest "putState ......... " (parserTest putStateTst)
    printTest "modifyState ...... " (parserTest modifyStateTst)
    printTest "satisfy .......... " (parserTest satisfyTst)
        where
            getStreamTst = statelessTest {
                parser = getStream,
                stream = "Hello!", expResult = "Hello!"
            }
            getStateTst = ParserTest {
                parser = getState,
                stream = " ",
                initState = [1, 2], expState = [1, 2],
                expResult = [1, 2]
            }
            putStateTst = ParserTest {
                parser = putState [1, 2],
                stream = " ",
                initState = [], expState = [1, 2],
                expResult = ()
            }
            modifyStateTst = ParserTest {
                parser = modifyState (+1),
                stream = " ",
                initState = 2, expState = 3,
                expResult = ()
            }
            satisfyTst = statelessTest {
                parser = char 'H',
                stream = "Hello!", expResult = 'H'
            }


main :: IO ()
main = parserTests
    >> combinatorsTests
