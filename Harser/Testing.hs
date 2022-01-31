{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields   #-}

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
        (_, Failure _)             -> False
        (State _ _ ru, Success a') -> (a == a') && (eu == ru)


parserTestFail :: ParserTest s u a -> Bool
parserTestFail (ParserTest p s iu _ _) =
    case runP p (State (StreamPos 0 0) s iu) of
        (_, Failure _) -> True
        (_, Success _) -> False
