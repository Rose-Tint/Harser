module Main where

import Harser.Char ()
import Harser.Combinators ()
import Harser.Lang ()
import Harser.Parser
import Harser.Stream ()
import Harser.Testing

import Examples.Calculator ()
import Examples.CSV ()


tests :: IO ()
tests = do
    putStrLn $ "getStream ..... " ++ show (parserTest getStreamTst)
    putStrLn $ "getState ...... " ++ show (parserTest getStateTst)
    putStrLn $ "putState ...... " ++ show (parserTest putStateTst)
    putStrLn $ "modifyState ... " ++ show (parserTest modifyStateTst)
    putStrLn $ "satisfyTst .... " ++ show (parserTest satisfyTst)
        where
            getStreamTst = ParserTest {
                parser = getStream,
                stream = "Hello, World!",
                initState = (), expState = (),
                expResult = "Hello, World!"
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
                initState = [],
                expState = [1, 2],
                expResult = ()
                }
            modifyStateTst = ParserTest {
                parser = modifyState (+1),
                stream = " ",
                initState = 2,
                expState = 3,
                expResult = ()
                }
            satisfyTst = ParserTest {
                parser = satisfy (== 'H'),
                stream = "Hello, World",
                initState = (), expState = (),
                expResult = 'H'
                }


main :: IO ()
main = tests

{-
-}
