module Harser.Parser (
    MonadFail(..),
    ParseError,
    StreamPos(StreamPos),
    ParseState(..),
    State(State),
    Parser(Parser),
    runP,
    getStream,
    getState,
    setState,
    amendState,
    getPosition,
    getPosLn,
    getPosCol,
    incStateLn,
    incStateCol,
    incSrcLn,
    incSrcCol,
    fulfill,
    parse,
    (<!), (<!>), (!>)
) where

import Prelude hiding (fail)

import Control.Applicative (Alternative(..))
import Control.Monad.Fail (MonadFail(..))
import Text.Printf (printf)

import Harser.Stream (Stream(..))


type ParseError = String


data StreamPos = StreamPos {
    linePos :: Int,
    colPos  :: Int
}


data State s u = State StreamPos s !u


data ParseState a
    = Failure ParseError
    | Success a
    deriving (Show, Eq)


newtype Parser s u a
    = Parser (State s u -> (State s u, ParseState a))


runP :: Parser s u a -> State s u -> (State s u, ParseState a)
runP (Parser a) s = a s


getStream :: Parser s u s
getStream = Parser (\st@(State _ ss _) -> (st, pure ss))


getState :: Parser s u u
getState = Parser (\st@(State _ _ su) -> (st, pure su))


setState :: u -> Parser s u ()
setState u = Parser (\(State p s _) -> (State p s u, pure ()))


amendState :: (u -> u) -> Parser s u ()
amendState f = Parser (\(State p s u) ->
    (State p s (f u), pure ()))


getPosition :: Parser s u StreamPos
getPosition = Parser (\st@(State sp _ _) ->  (st, pure sp))


getPosLn :: Parser s u Int
getPosLn = linePos <$> getPosition


getPosCol :: Parser s u Int
getPosCol = colPos <$> getPosition


incStateLn :: State s u -> State s u
incStateLn (State p s u) = State (incSrcLn p) s u


incStateCol :: State s u -> State s u
incStateCol (State p s u) = State (incSrcCol p) s u


incSrcLn :: StreamPos -> StreamPos
incSrcLn (StreamPos ln col) = StreamPos (ln + 1) col


incSrcCol :: StreamPos -> StreamPos
incSrcCol (StreamPos ln col) = StreamPos ln (col + 1)


failFrom :: State s u -> String -> ParseState a
failFrom (State (StreamPos ln col) _ _) msg =
    Failure $ printf "[L%d:C%d] %s" ln col msg


-- | always consumes input when not empty
fulfill :: (Stream s t) => (t -> Bool) -> Parser s u t
fulfill f = Parser (\s@(State sp ss su) -> case uncons ss of
    Nothing      -> (s, failFrom s "empty")
    Just (t, ts) -> let s' = State (incSrcCol sp) ts su
        in if f t then
            (s', Success t)
        else
            (s', failFrom s (show t ++ " did not fulfill")))


parse :: Parser s u a -> s -> u -> ParseState a
parse p s u = snd $ runP p (State (StreamPos 0 0) s u)


infix 2 <!>
-- | replaces the error message
(<!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <!> e = Parser (\s -> case a s of
    (s', Failure _) -> (s', failFrom s' e)
    (s', Success x) -> (s', Success x))


infix 2 <!
-- | prepends to the error message
(<!) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <! e = Parser (\s -> case a s of
    (s', Failure e') -> (s', Failure (e ++ e'))
    (s', Success x)  -> (s', Success x))


infix 2 !>
-- | appends to the error message
(!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) !> e = Parser (\s -> case a s of
    (s', Failure e') -> (s', Failure (e' ++ e))
    (s', Success x) -> (s', Success x))


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
    empty = Parser (\s -> (s, failFrom s "empty"))
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
    empty = fail "[]"
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
    fail e = Parser (\s -> (s, failFrom s e))


instance MonadFail ParseState where
    fail = Failure
