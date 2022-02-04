{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields   #-}

{-|
Module      : Harser.Testing
Description : Parser testing things
-}

module Harser.Testing (
    ParserTest(..),
    statelessTest,
    parserTest,
    parserTestFail,
) where

import Harser.Parser (
    Parser(..),
    State(..),
    ParseState(..),
    runP,
    StreamPos(StreamPos)
    )


-- | @'ParserTest' s u a@ represents a parser test
-- case with parser type @'Parser' s u a@
data ParserTest s u a = ParserTest {
    -- |The parser being tested
    parser    :: Parser s u a,
    -- |The input stream
    stream    :: s,
    -- |Initial user state
    initState :: u,
    -- |Expected post-parse user state
    expState  :: u,
    -- |Expected post-parse result
    expResult :: a
}


-- |Initializes a ParserTest with a state type of
-- @()@, initial state @()@, and expected state
-- @()@
statelessTest :: ParserTest s () a
statelessTest = ParserTest { initState = (), expState = () }


-- |Returns @'True'@ if the result of the parser
-- and the ending user state are equal to their
-- respective expected values.
parserTest :: (Eq u, Eq a) => ParserTest s u a -> Bool
parserTest (ParserTest p s iu eu a) =
    case runP p (State (StreamPos 0 0) s iu) of
        (_, Failure _)             -> False
        (State _ _ ru, Success a') -> (a == a') && (eu == ru)


-- |Returns @'True'@ upon failure, and @'False'@
-- otherwise
parserTestFail :: ParserTest s u a -> Bool
parserTestFail (ParserTest p s iu _ _) =
    case runP p (State (StreamPos 0 0) s iu) of
        (_, Failure _) -> True
        (_, Success _) -> False
