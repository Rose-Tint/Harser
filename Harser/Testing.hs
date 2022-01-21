{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields   #-}

module Harser.Testing (
    ParserTest(..),
    statelessTest,
    parserTest,
    parserTestFail,
    parserTestState,
    parserTestResult
) where

import Harser.Parser (
    Parser(..),
    State(..),
    ParseState(..),
    runP,
    StreamPos(StreamPos)
    )


data ParserTest s u a = ParserTest {
    parser    :: Parser s u a,
    stream    :: s,
    initState :: u,
    expState  :: u,
    expResult :: a
}


statelessTest :: ParserTest s () a
statelessTest = ParserTest { initState = (), expState = () }


parserTest :: (Eq u, Eq a) => ParserTest s u a -> Bool
parserTest (ParserTest p s iu eu a) =
    case runP p (State (StreamPos 0 0) s iu) of
        (_, Failure _)           -> False
        (State _ _ ru, Success a') -> (a == a') && (eu == ru)


parserTestFail :: ParserTest s u a -> Bool
parserTestFail (ParserTest p s iu _ _) =
    case runP p (State (StreamPos 0 0) s iu) of
        (_, Failure _) -> True
        (_, Success _) -> False


parserTestState :: (Eq u) => ParserTest s u a -> Bool
parserTestState (ParserTest p s iu eu _) =
    case runP p (State (StreamPos 0 0) s iu) of
        (State _ _ _, Failure _)  -> False
        (State _ _ res, Success _) -> (res == eu)


parserTestResult :: (Eq a) => ParserTest s u a -> Bool
parserTestResult (ParserTest p s iu _ a) =
    case runP p (State (StreamPos 0 0) s iu) of
        (State _ _ _, Failure _)  -> False
        (State _ _ _, Success res) -> (res == a)
