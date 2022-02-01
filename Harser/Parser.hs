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


-- |@ParseState a@ represents the result of a parser,
-- or an error message
data ParseState a
    -- |Represents a parsing failure, holding an
    -- error message
    = Failure ParseError
    -- |Represents a parsing success, holding the
    -- parsed value
    | Success a
    deriving (Show, Eq)


-- |@Parser s u a@ is a monadic parser with stream
-- type @s@, user state type @u@, and return type
-- @a@
newtype Parser s u a
    = Parser (State s u -> (State s u, ParseState a))


-- |@runP p s@ runs the parser @p@ with the initial
-- state @s@
runP :: Parser s u a -> State s u -> (State s u, ParseState a)
runP (Parser a) s = a s


-- |Returns the current stream. Always successful.
getStream :: Parser s u s
getStream = Parser $ \st@(State _ ss _) -> (st, pure ss)


-- |@fgetState f@ returns the current user state,
-- applying f only to the return state. Does not
-- affect the ongoing state.
fgetState :: (u -> a) -> Parser s u a
fgetState f = f <$> getState


-- |Returns the current user state.
getState :: Parser s u u
getState = Parser $ \st@(State _ _ su) -> (st, pure su)


-- |@setState u@ sets the current user state to @u@,
-- disregarding the old state, and returning @()@
setState :: u -> Parser s u ()
setState u = Parser $ \(State p s _) -> (State p s u, pure ())


-- |@amendState f@ applies f to the current user
-- state, and returns @()@
amendState :: (u -> u) -> Parser s u ()
amendState f = Parser $ \(State p s u) ->
    (State p s (f u), pure ())


-- |Returns the current stream position
getPosition :: Parser s u StreamPos
getPosition = Parser $ \st@(State sp _ _) -> (st, pure sp)


-- |Gets the current line from the stream
-- position
getPosLn :: Parser s u Int
getPosLn = getSrcLn <$> getPosition


-- |Gets the current column from the stream
-- position
getPosCol :: Parser s u Int
getPosCol = getSrcCol <$> getPosition


-- |@fulfull f@ consumes one token from the
-- current stream. Returns the parsed token
-- if the token satisfies @f@, and fails
-- otherwise
fulfill :: (Stream s t) => (t -> Bool) -> Parser s u t
fulfill f = Parser $ \s@(State sp ss su) -> case uncons ss of
    Nothing      -> (s, fail "empty")
    Just (t, ts) -> let s' = State (incSrcCol sp) ts su
        in if f t then
            (s', Success t)
        else
            (s', fail "fulfill")


-- |Attempts to consume a token from the stream.
-- Fails if the stream is empty, otherwise
-- returns @()@
pNext :: (Stream s t) => Parser s u ()
pNext = Parser $ \s@(State _ ss _) -> case uncons ss of
    Nothing -> (s, fail "empty")
    Just _  -> (incCol s, pure ())


-- |@parse p s u@ parses the input stream @s@
-- using the parser @p@ and initial user state
-- @u@. Returns a the resulting 'ParseState'
-- value.
parse :: Parser s u a -> s -> u -> ParseState a
parse p s u = snd $ runP p (State (StreamPos 0 0) s u)


-- |Same as @'parse'@, but returns a tuple with
-- the resulting user state as the first element,
-- and the resulting @'ParseState'@ value as the
-- second element
parse' :: Parser s u a -> s -> u -> (u, ParseState a)
parse' p s u = (getStateUser st, res)
    where
        (st, res) = runP p (State (StreamPos 0 0) s u)


-- |Same as @'parse'@, but returns a tuple with
-- the resulting underlying state as the first
-- element, and the resulting @'ParseState'@
-- value as the second element
parse'' :: Parser s u a -> s -> u -> (State s u, ParseState a)
parse'' p s u = runP p (State (StreamPos 0 0) s u)


-- |@'joinFails' e1 e2@ concatenates e1 and e2
-- with a new-line character between them.
joinFails :: ParseError -> ParseError -> ParseState a
joinFails e1 e2 = fail $ e1 ++ "\n" ++ e2


-- |@p1 '<?>' p2@ runs @p1@ on the stream. If @p1@
-- fails the state is reset, and result of @p2@ is
-- returned; otherwise the result of @p1@ is returned
infixr 1 <?>
(<?>) :: Parser s u a -> Parser s u a -> Parser s u a
(Parser lf) <?> (Parser rf) = Parser $ \s -> case lf s of
        (_, Failure _)  -> rf s
        (s', Success x) -> (s', Success x)


-- |@p '<!>' msg@ runs @p@ on the stream. If @p@
-- fails, the failure message is replaced with msg.
-- If replacement is undesirable, see '(<!)' or '(!>)'
infix 2 <!>
(<!>) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <!> e = Parser $ \s -> case a s of
    (s', Failure _) -> (s', fail e)
    (s', Success x) -> (s', Success x)


-- |Works like '(<!>)', but prepends to the failure
-- message instead of replacing it
infix 2 <!
(<!) :: Parser s u a -> ParseError -> Parser s u a
(Parser a) <! e = Parser $ \s -> case a s of
    (s', Failure e') -> (s', joinFails e' e)
    (s', Success x)  -> (s', Success x)


-- |Works like '(<!>)', but appends to the failure
-- message instead of replacing it
infix 2 !>
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
