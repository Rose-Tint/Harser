module Harser.Combinators where

import Control.Applicative

import Parser


zeroOrOne :: Parser a -> Parser (Maybe a)
zeroOrOne (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success Nothing)
    (s', Success a') -> (s', Success (Just a')))


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (Parser f) = Parser go where
    go s = case f s of
        (_, Failure _)  -> (s, Success [])
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


option :: Parser a -> a -> Parser a
option (Parser a) o = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success o)
    (s', Success x) -> (s', Success x))


optional :: Parser a -> Parser ()
optional (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success ())
    (s', Success _) -> (s', Success ()))


-- | 1+
sepBy :: Parser s -> Parser a -> Parser [a]
sepBy s p = do
    x <- p
    xs <- zeroOrOne (s *> p)
    return (x:xs)


-- | 0+
sepBy' :: Parser s -> Parser a -> Parser [a]
sepBy' s p = sepBy s p <|> pure []


count :: Int -> Parser a -> Parser [a]
count 0 p = return []
count n p = do
    x <- p
    xs <- count (n - 1) p
    return (x:xs)


skip :: Parser a -> Parser ()
skip p@(Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success ())
    (s', Success _) -> runParser (skip p) s'


skipn :: Int -> Parser a -> Parser ()
skipn 0 p = Parser (\s -> let (s', _) = a s
    in (s', _) -> (s', Success ())
skipn n p@(Parser a) = Parser (\s -> case a s of
    (_, Failure _) -> (s, Success ())
    (_, Success _) -> skipn (n - 1) p


choose :: [Parser a] -> Parser a
choose ps = foldr (<|>) empty ps


wrapped :: Parser a -> Parser b -> Parser b
wrapped s p = s *> p <* s


between :: Parser a -> Parser b -> Parser c -> Parser b
between ls p rs = ls *> p <* rs
