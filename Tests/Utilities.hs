module Main where

import System.Exit

import Harser.Utilities
import Harser.Testing

import Tests.TestTools


main :: IO ()
main = do
    _ <- putStrLn "Harser.Utilities"
    t1 <- doTest "fractional" test_fractional
    t2 <- doTest "integral" test_integral
    if and [t1, t2] then
        exitSuccess
    else
        exitFailure
        where
            test_fractional = statelessTest {
                parser = fractional, 
                stream = "123.456",
                expResult = 123.456 :: Double
            }
            test_integral = statelessTest {
                parser = integral,
                stream = "123456",
                expResult = 123456 :: Integer
            }
