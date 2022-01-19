module Harser.Testing (
    ParserTest(..),
    parserTest,
    parserTestFail,
    parserTestState,
    parserTestResult
) where

import Harser.Parser (Parser(..), State(..), ParseState(..), runP)


data ParserTest s u a = ParserTest {
    parser    :: !(Parser s u a),
    stream    :: !s,
    initState :: !u,
    expState  :: !u,
    expResult :: !a
}


parserTest :: (Eq u, Eq a) => ParserTest s u a -> Bool
parserTest (ParserTest p s iu eu a) = case runP p (State s iu) of
    (_, Failure _)           -> False
    (State _ ru, Success a') -> (a == a') && (eu == ru)


parserTestFail :: ParserTest s u a -> Bool
parserTestFail (ParserTest p s iu _ _) = case runP p (State s iu) of
    (_, Failure _) -> True
    (_, Success _) -> False


parserTestState :: (Eq u) => ParserTest s u a -> Bool
parserTestState (ParserTest p s iu eu _) = case runP p (State s iu) of
    (State _ _, Failure _)  -> False
    (State _ res, Success _) -> (res == eu)


parserTestResult :: (Eq a) => ParserTest s u a -> Bool
parserTestResult (ParserTest p s iu _ a) = case runP p (State s iu) of
    (State _ _, Failure _)  -> False
    (State _ _, Success res) -> (res == a)
