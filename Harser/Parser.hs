module Harser.Parser (
    module Harser.State,
    MonadFail(..),
    ParseState(..),
    Parser(Parser),
    runP,
    getStream,
    fgetState,
    getState,
    setState,
    amendState,
    getPosition,
    getPosLn,
    getPosCol,
    fulfill,
    pNext,
    parse, parse', parse'',
    (<?>),
    (<!), (<!>), (!>)
) where

import Prelude hiding (fail)

import Control.Applicative (Alternative(..))
import Control.Monad.Fail (MonadFail(..))

import Harser.Stream (Stream(..))
import Harser.State


data ParseState a
    = Failure ParseError
    | Success a
    deriving (Show, Eq)


newtype Parser s u a
    = Parser (State s u -> (State s u, ParseState a))


runP :: Parser s u a -> State s u -> (State s u, ParseState a)
runP (Parser a) s = a s


getStream :: Parser s u s
getStream = Parser $ \st@(State _ ss _) -> (st, pure ss)


setStream :: s -> Parser s u ()
setStream s = Parser $ \(State p _ u) -> (State p s u, pure ())


fgetState :: (u -> a) -> Parser s u a
fgetState f = f <$> getState


getState :: Parser s u u
getState = Parser $ \st@(State _ _ su) -> (st, pure su)


setState :: u -> Parser s u ()
setState u = Parser $ \(State p s _) -> (State p s u, pure ())


amendState :: (u -> u) -> Parser s u ()
amendState f = Parser $ \(State p s u) ->
    (State p s (f u), pure ())


getPosition :: Parser s u StreamPos
getPosition = Parser $ \st@(State sp _ _) ->  (st, pure sp)


getPosLn :: Parser s u Int
getPosLn = getSrcLn <$> getPosition


getPosCol :: Parser s u Int
getPosCol = getSrcCol <$> getPosition


-- | always consumes input when not empty
fulfill :: (Stream s t) => (t -> Bool) -> Parser s u t
fulfill f = Parser $ \s@(State sp ss su) -> case uncons ss of
    Nothing      -> (s, fail "empty")
    Just (t, ts) -> let s' = State (incSrcCol sp) ts su
        in if f t then
            (s', Success t)
        else
            (s', fail "fulfill")


-- | always consumes input when not empty
pNext :: (Stream s t) => Parser s u ()
pNext = Parser $ \s@(State _ ss _) -> case uncons ss of
    Nothing -> (s, fail "empty")
    Just _  -> (incCol s, pure ())


-- | @parse p s u@ can be read as "parse @s@ using the parser
-- | @p@, with state @u@". Returns @Success result@ on success,
-- | or @Failure msg@ on failure. Use @unlines msg@ to get the
-- | list of error messages.
parse :: Parser s u a -> s -> u -> ParseState a
parse = snd . parse''


-- | same as @parse@, but returns a tuple with the resulting
-- | user state as the first element, and a the result of
-- | the parser as the second element (in the same form as
-- | @parse@).
parse' :: Parser s u a -> s -> u -> (u, ParseState a)
parse' p s u = (getStateUser st, res)
    where
        (st, res) = runP p (State (StreamPos 0 0) s u)


-- | same as @parse@, but returns the full state, which includes
-- | the stream, stream position, and user state resulting from
-- | the parser
parse'' :: Parser s u a -> s -> u -> (State s u, ParseState a)
parse'' p s u = runP p (State (StreamPos 0 0) s u)


joinFails :: ParseError -> ParseError -> ParseState a
joinFails e1 e2 = fail $ e1 ++ "\n" ++ e2


infixr 1 <?>
-- | like <|>, but with backtracking
(<?>) :: Parser s u a -> Parser s u a -> Parser s u a
(Parser lf) <?> (Parser rf) = Parser $ \s -> case lf s of
        (_, Failure _)  -> rf s
        (s', Success x) -> (s', Success x)


infix 2 <!>
-- | replaces the error message
(<!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <!> e = Parser $ \s -> case a s of
    (s', Failure _) -> (s', fail e)
    (s', Success x) -> (s', Success x)


infix 2 <!
-- | prepends to the error message
(<!) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <! e = Parser $ \s -> case a s of
    (s', Failure e') -> (s', joinFails e' e)
    (s', Success x)  -> (s', Success x)


infix 2 !>
-- | appends to the error message
(!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) !> e = Parser $ \s -> case a s of
    (s', Failure e') -> (s', joinFails e e')
    (s', Success x) -> (s', Success x)


instance Functor (Parser s u) where
    fmap f (Parser a) = Parser $ \s ->
        let (s', ps) = a s in (s', fmap f ps)


instance Functor ParseState where
    fmap f (Success x) = Success (f x)
    fmap _ (Failure e) = Failure e


instance Applicative (Parser s u) where
    pure a = Parser $ \s -> (s, Success a)
    Parser f <*> Parser x = Parser $ \s -> case f s of
        (s', Failure e)  -> (s', Failure e)
        (s', Success f') -> case x s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success x') -> (s'', Success (f' x'))


instance Applicative ParseState where
    pure a = Success a
    Success f <*> Success x = Success (f x)
    Failure e1 <*> Failure e2 = joinFails e1 e2
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e


instance Alternative (Parser s u) where
    empty = fail "empty"
    Parser lf <|> Parser rf = Parser $ \s -> case lf s of
        (s', Failure _) -> rf s'
        (s', Success x) -> (s', Success x)
    many (Parser f) = Parser fn where
        fn s = case f s of
            (_, Failure _)  -> (s, Success [])
            (s', Success a) -> case fn s' of 
                (s'', Failure e) -> (s'', Failure e)
                (s'', Success as) -> (s'', Success (a:as))
    some p = (:) <$> p <*> many p


instance Alternative ParseState where
    empty = fail "empty"
    le@(Success _) <|> _ = le
    Failure _ <|> ri@(Success _) = ri
    Failure e1 <|> Failure e2 = joinFails e1 e2


instance Monad (Parser s u) where
    return = pure
    (>>) = (*>)
    Parser a >>= f = Parser $ \s -> case a s of
        (s', Failure e) -> (s', Failure e)
        (s', Success x) -> runP (f x) s'


instance Monad ParseState where
    Success a >>= f = f a
    Failure e >>= _ = Failure e


instance MonadFail (Parser s u) where
    fail e = Parser $ \s -> (s, fail e)


instance MonadFail ParseState where
    fail = Failure
