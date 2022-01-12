module Harser.Parser where

import Control.Applicative
import Control.Monad.Fail
import qualified Data.Text


type ParseError = String
type Stream = Data.Text.Text


data StreamState a
    = Failure ParseError
    | Success a
    deriving (Show, Eq)


newtype Parser a = Parser {
    unParser :: Stream -> (Stream, StreamState a)
}


-- | replaces the error message
(<!>) :: Parser a -> ParseError -> Parser a
(Parser a) <!> e = Parser (\s -> case a s of
    (s', Failure _) -> (s', Failure e)
    (s', Success x) -> (s', Success x))
infix 8 <!>


-- | prepends to the error message
(<!) :: Parser a -> ParseError -> Parser a
(Parser a) <! e = Parser (\s -> case a s of
    (s', Failure e') -> (s', Failure (e ++ e'))
    (s', Success x)  -> (s', Success x))
infix 8 <!


-- | appends to the error message
(!>) :: Parser a -> ParseError -> Parser a
(Parser a) !> e = Parser (\s -> case a s of
    (s', Failure e') -> (s', Failure (e' ++ e))
    (s', Success x) -> (s', Success x))
infix 8 !>


(<?>) :: Parser a -> Parser a -> Parser a
lp <?> rp = try lp <|> rp
infixl 7 <?>


failure :: String -> Parser a
failure e = Parser (\s -> (s, Failure e))


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser (\s -> (Data.Text.tail s,
    if Data.Text.null s then
        Failure "empty"
    else let c = Data.Text.head s
        in if f c then
            Success c
        else Failure ("did not satisfy: '" ++ (c:"'"))))


-- | restores stream to previous state on failure
try :: Parser a -> Parser a
try (Parser f) = Parser (\s -> case f s of
    (_, Failure e)  -> (s, Failure e)
    (s', Success x) -> (s', Success x))


parseStr :: Parser a -> String -> StreamState a
parseStr p s = parse p (Data.Text.pack s)


parse :: Parser a -> Stream -> StreamState a
parse (Parser a) = snd . a


prefix :: Parser a -> Parser a -> Parser a
prefix s p = s *> p


maybePrefix :: Parser a -> Parser a -> Parser a
maybePrefix s p = try s *> p


suffix :: Parser a -> Parser a -> Parser a
suffix p s = p <* s


maybeSuffix :: Parser a -> Parser a -> Parser a
maybeSuffix p s = p <* try s


instance Functor Parser where
    fmap f (Parser a) = Parser (\s -> case a s of
        (s', Failure e) -> (s', Failure e)
        (s', Success x) -> (s', Success (f x)))


instance Functor StreamState where
    fmap f (Success x) = Success (f x)
    fmap _ (Failure e) = Failure e


instance Applicative Parser where
    pure a = Parser (\s -> (s, Success a))
    Parser f <*> Parser x = Parser (\s -> case f s of
        (s', Failure e)  -> (s', Failure e)
        (s', Success f') -> case x s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success x') -> (s'', Success (f' x')))


instance Applicative StreamState where
    pure a = Success a
    Success f <*> Success x = Success (f x)
    Failure e0 <*> Failure e1 = Failure ("left: " ++ e0
                                      ++ "right: " ++ e1)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e


instance Alternative Parser where
    empty = Parser (\s -> (s, Failure "empty"))
    Parser lf <|> Parser rf = Parser (\s -> case lf s of
        (s', Failure _) -> rf s'
        (s', Success x) -> (s', Success x))
    -- many = oneOrMore
    -- some = zeroOrMore


instance Monad Parser where
    return = pure
    Parser a >>= f = Parser (\s -> case a s of
        (s', Failure e) -> (s', Failure e)
        (s', Success x) -> case (unParser $ f x) s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success x') -> (s'', Success x'))


instance Monad StreamState where
    Success a >>= f = f a
    Failure e >>= _ = Failure e


instance MonadFail Parser where
    fail e = Parser (\s -> (s, Failure e))


instance MonadFail StreamState where
    fail s = Failure s
