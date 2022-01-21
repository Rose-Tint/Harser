module Harser.Parser (
    ParseError,
    StreamPos(StreamPos),
    ParseState(..),
    State(State),
    Parser(Parser),
    runP,
    getStream,
    getState,
    putState,
    modifyState,
    getPosition,
    getPosLn,
    getPosCol,
    incStateLn,
    incStateCol,
    incSrcLn,
    incSrcCol,
    satisfy,
    parse,
    failFrom,
    (<!), (<!>), (!>)
) where

import Control.Applicative (Alternative(..))
import Control.Monad.Fail (MonadFail(..))
import Text.Printf (printf)

import Harser.Stream (Stream(..))


type ParseError = String


data StreamPos = StreamPos {
    linePos :: Integer,
    colPos  :: Integer
}


data State s u = State {
    streamPos :: StreamPos,
    stream    :: s,
    userState :: !u
}


data ParseState a = Failure ParseError | Success a
    deriving (Show, Eq)


newtype Parser s u a = Parser {
    unParser :: State s u -> (State s u, ParseState a)
}


runP :: Parser s u a -> State s u -> (State s u, ParseState a)
runP p st = (unParser p) st


getStream :: Parser s u s
getStream = Parser (\s -> (s, pure (stream s)))


getState :: Parser s u u
getState = Parser (\s -> (s, pure (userState s)))


putState :: u -> Parser s u ()
putState u = Parser (\(State p s _) -> (State p s u, pure ()))


modifyState :: (u -> u) -> Parser s u ()
modifyState f = Parser (\(State p s u) ->
    (State p s (f u), pure ()))


getPosition :: Parser s u StreamPos
getPosition = Parser (\s ->  (s, pure $ streamPos s))


getPosLn :: Parser s u Integer
getPosLn = linePos <$> getPosition


getPosCol :: Parser s u Integer
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


-- | always consumes input when not empty
satisfy :: (Stream s t) => (t -> Bool) -> Parser s u t
satisfy f = Parser (\s@(State sp ss su) -> case uncons ss of
    Nothing      -> (s, failFrom s "empty")
    Just (t, ts) -> let s' = State (incSrcCol sp) ts su
        in if f t then
            (s', Success t)
        else
            (s', failFrom s' (show t ++ " did not satisfy")))


parse :: Parser s u a -> s -> u -> ParseState a
parse p s u = snd $ runP p (State (StreamPos 0 0) s u)


instance Functor (Parser s u) where
    fmap f (Parser a) = Parser (\s -> -- ps is (P)arser (S)tate
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
    empty = Failure "[]"
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
