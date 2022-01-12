module Harser.Parser where

import Control.Applicative
import Control.Monad.Fail

import Harser.Stream


type ParseError = String


data ParseState a
    = Failure ParseError
    | Success a
    deriving (Show, Eq)


newtype Parser s u a = Parser { unParser :: s -> (s, ParseState a) }


runParser :: Parser s u a -> s -> (s, ParseState a)
runParser (Parser a) = a


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


infixr 7 <?>
-- | like <|> but trys left arg. <?> is right associative
(<?>) :: Parser s u a -> Parser s u a -> Parser s u a
lp <?> rp = try lp <|> rp


satisfy :: (Stream s t) => (t -> Bool) -> Parser s u t
satisfy f = Parser (\s -> case uncons s of
    Nothing      -> (s, Failure "empty")
    Just (t, ts) -> if f t then
            (ts, Success t)
        else
            (ts, Failure "did not satisfy"))


-- | restores stream to previous state on failure
try :: Parser s u a -> Parser s u a
try (Parser f) = Parser (\s -> case f s of
    (_, Failure e)  -> (s, Failure e)
    (s', Success x) -> (s', Success x))


instance Functor (Parser s u) where
    fmap f (Parser a) = Parser (\s -> case a s of
        (s', Failure e) -> (s', Failure e)
        (s', Success x) -> (s', Success (f x)))


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
    Failure e0 <*> Failure e1 = Failure ("left: " ++ e0
                                      ++ "right: " ++ e1)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e


instance Alternative (Parser s u) where
    empty = Parser (\s -> (s, Failure "empty"))
    Parser lf <|> Parser rf = Parser (\s -> case lf s of
        (s', Failure _) -> rf s'
        (s', Success x) -> (s', Success x))
    -- many = oneOrMore
    -- some = zeroOrMore


instance Monad (Parser s u) where
    return = pure
    Parser a >>= f = Parser (\s -> case a s of
        (s', Failure e) -> (s', Failure e)
        (s', Success x) -> case (unParser $ f x) s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success x') -> (s'', Success x'))


instance Monad ParseState where
    Success a >>= f = f a
    Failure e >>= _ = Failure e


instance MonadFail (Parser s u) where
    fail e = Parser (\s -> (s, Failure e))


instance MonadFail ParseState where
    fail s = Failure s
