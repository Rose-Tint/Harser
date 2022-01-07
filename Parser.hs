module Parser where

import Control.Applicative
import qualified Data.Text as T
import System.IO
import Debug.Trace


type ParseError = String


data StreamState a
    = Failure ParseError
    | Success a
    deriving (Show, Eq)


newtype Parser a = Parser {
    unParser :: T.Text -> (T.Text, StreamState a)
}


instance Functor Parser where
    fmap f (Parser a) = Parser (\s ->
        let (s', a') = a s in (s', fmap f a'))


instance Functor StreamState where
    fmap f (Success x) = Success (f x)
    fmap f (Failure e) = Failure e


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
    Failure e0 <*> Failure e1 = Failure ("StreamState <*>:"
        ++ "\n\tleft:" ++ e0
        ++ "\n\tright:" ++ e1)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e


instance Alternative Parser where
    empty = Parser (\s -> (s, Failure "empty"))
    Parser lf <|> Parser rf = Parser (\s -> case lf s of
        (s', Failure e) -> rf s'
        (s', Success x) -> (s', Success x))
    many = oneOrMore
    some = zeroOrMore


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


runParser :: Parser a -> T.Text -> (T.Text, StreamState a)
runParser (Parser a) s = a s


-- alternative to Maybe would be to use '\0' if zero
zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne (Parser a) = Parser (\s -> case a s of
    (s', Failure e) -> (s', Success Nothing)
    (s', Success a) -> (s', Success (Just a)))


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (Parser f) = Parser go where
    go s = case f s of
        (_, Failure e)  -> (s, Success [])
        (s', Success a) -> case go s' of 
            (s'', Failure e) -> (s'', Failure e)
            (s'', Success as) -> (s'', Success (a:as))


oneOrMore :: Parser a -> Parser [a]
oneOrMore (Parser f) = Parser (\s -> case f s of
    (s', Failure e) -> (s', Failure e)
    (s', Success a) -> let (Parser f') = zeroOrMore (Parser f)
        in case f' s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success as) -> (s'', Success (a:as)))


char :: Char -> Parser Char
char c = satisfy (== c)


string :: String -> Parser String
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser (\s -> (T.tail s,
    if T.null s then
        Failure "empty"
    else let c = T.head s
        in if f c then
            Success c
        else Failure "did not satisfy"))


satisfyNext :: (Char -> Bool) -> Parser Char
satisfyNext f = zeroOrMore (satisfy (not . f)) >> satisfy f


try :: Parser a -> Parser a
try (Parser f) = Parser (\s -> case f s of
    (_, Failure e)  -> (s, Failure e) -- restores stream to previous state
    (s', Success x) -> (s', Success x))


parseStr :: Parser a -> String -> StreamState a
parseStr p s = parse p (T.pack s)


parse :: Parser a -> T.Text -> StreamState a
parse (Parser a) = snd . a
