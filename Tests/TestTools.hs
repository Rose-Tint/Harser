module Tests.TestTools where

import Text.Printf

import Harser.Testing


doTest :: (Eq u, Eq a)
       => String -> ParserTest s u a -> IO Bool
doTest s t = do
    let b = parserTest t
    let testResults = if b then "2mPass" else "1mFail"
    _ <- printf "\t%s%s\027[3%s\027[0m\n" s
            (replicate spaceLen ' ')
            testResults
    return b
        where
            spaceLen = 30 - 8 - 4 - length s
