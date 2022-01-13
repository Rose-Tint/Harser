module Harser.Parser (
    ParseError,
    ParseState(..),
    State(..),
    Parser(..),
    runP,
    getInput,
    getState,
    putState,
    modifyState,
    (<!>), (<!), (!>),
    satisfy,
    try,
    exactly,
    parse
) where

import Control.Applicative (Alternative(..))
import Control.Monad.Fail (MonadFail(..))

import Harser.Stream (Stream(..))


type ParseError = String


data ParseState a
    = Failure ParseError
    | Success a
    deriving (Show, Eq)


data State s u = State {
    stateStream :: s,
    stateUser :: !u
}


newtype Parser s u a = Parser {
    unParser :: State s u -> (State s u, ParseState a)
}


runP :: Parser s u a -> State s u -> (State s u, ParseState a)
runP p st = (unParser p) st


getInput :: Parser s u s
getInput = Parser (\s -> (s, pure (stateStream s)))


getState :: Parser s u u
getState = Parser (\s -> (s, pure (stateUser s)))


putState :: u -> Parser s u ()
putState u = Parser (\(State ss _) -> (State ss u, pure ()))


modifyState :: (u -> u) -> Parser s u ()
modifyState f = Parser (\(State s u) -> (State s (f u), pure ()))


infix 8 <!>
-- | replaces the error message
(<!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <!> e = Parser (\s -> case a s of
    (s', Failure _) -> (s', Failure e)
    (s', Success x) -> (s', Success x))


infix 8 <!
-- | prepends to the error message
(<!) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <! e = Parser (\s -> case a s of
    (s', Failure e') -> (s', Failure (e ++ e'))
    (s', Success x)  -> (s', Success x))


infix 8 !>
-- | appends to the error message
(!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) !> e = Parser (\s -> case a s of
    (s', Failure e') -> (s', Failure (e' ++ e))
    (s', Success x) -> (s', Success x))


-- | i dont think this is right
satisfy :: (Stream s t) => (t -> Bool) -> Parser s u t
satisfy f = Parser (\s@(State ss su) -> case uncons ss of
    Nothing      -> (s, Failure "empty")
    Just (t, ts) -> if f t then
            (State ts su, Success t)
        else
            (State ts su, Failure "did not satisfy"))


-- | restores stream to previous state on failure
try :: Parser s u a -> Parser s u a
try (Parser f) = Parser (\s -> case f s of
    (_, Failure e)  -> (s, Failure e)
    (s', Success x) -> (s', Success x))


exactly :: (Eq t, Stream s t) => t -> Parser s u t
exactly t = satisfy (== t)


parse :: Parser s u a -> s -> u -> ParseState a
parse (Parser a) s u = snd (a (State s u))


instance Functor (Parser s u) where
    fmap f (Parser a) = Parser (\s ->
        let (s', ps) = a s in (s', fmap f ps))


instance Functor ParseState where
    fmap f (Success x) = Success (f x)
    fmap _ (Failure e) = Failure e


instance Applicative (Parser s u) where
    pure a = Parser (\s -> (s, Success a))
    Parser f <*> Parser x = Parser (\s -> case f s of
        (s', Failure e)  -> (s', Failure e)
        (s', Success f') -> case x s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success x') -> (s'', Success (f' x')))


instance Applicative ParseState where
    pure a = Success a
    Success f <*> Success x = Success (f x)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e


instance Alternative (Parser s u) where
    empty = Parser (\s -> (s, Failure "empty"))
    Parser lf <|> Parser rf = Parser (\s -> case lf s of
        (s', Failure _) -> rf s'
        (s', Success x) -> (s', Success x))
    many (Parser f) = Parser fn where
        fn s = case f s of
            (_, Failure _)  -> (s, Success [])
            (s', Success a) -> case fn s' of 
                (s'', Failure e) -> (s'', Failure e)
                (s'', Success as) -> (s'', Success (a:as))
    some (Parser f) = Parser (\s -> case f s of
        (s', Failure e) -> (s', Failure e)
        (s', Success a) -> let (Parser f') = many (Parser f)
            in case f' s' of
                (s'', Failure e)  -> (s'', Failure e)
                (s'', Success as) -> (s'', Success (a:as)))


instance Alternative ParseState where
    empty = Failure ""
    le@(Success _) <|> _ = le
    Failure _ <|> ri@(Success _) = ri
    le@(Failure _) <|> _ = le


instance Monad (Parser s u) where
    return = pure
    (>>) = (*>)
    Parser a >>= f = Parser (\s -> case a s of
        (s', Failure e) -> (s', Failure e)
        (s', Success x) -> runP (f x) s')


instance Monad ParseState where
    Success a >>= f = f a
    Failure e >>= _ = Failure e


instance MonadFail (Parser s u) where
    fail e = Parser (\s -> (s, Failure e))


instance MonadFail ParseState where
    fail = Failure
