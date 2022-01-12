module Harser.Combinators where

import Control.Applicative

import Harser.Parser
import Harser.Stream


zeroOrOne :: Parser s u a -> Parser s u (Maybe a)
zeroOrOne (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success Nothing)
    (s', Success a') -> (s', Success (Just a')))


zeroOrMore :: Parser s u a -> Parser s u [a]
zeroOrMore (Parser f) = Parser go where
    go s = case f s of
        (_, Failure _)  -> (s, Success [])
        (s', Success a) -> case go s' of 
            (s'', Failure e) -> (s'', Failure e)
            (s'', Success as) -> (s'', Success (a:as))


oneOrMore :: Parser s u a -> Parser s u [a]
oneOrMore (Parser f) = Parser (\s -> case f s of
    (s', Failure e) -> (s', Failure e)
    (s', Success a) -> let (Parser f') = zeroOrMore (Parser f)
        in case f' s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success as) -> (s'', Success (a:as)))


option :: Parser s u a -> a -> Parser s u a
option (Parser a) o = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success o)
    (s', Success x) -> (s', Success x))


optional :: Parser s u a -> Parser s u ()
optional (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success ())
    (s', Success _) -> (s', Success ()))


-- | 1+
sepBy :: Parser s u s -> Parser s u a -> Parser s u [a]
sepBy s p = do
    x <- p
    xs <- zeroOrMore (s *> p)
    return (x:xs)


-- | 0+
sepBy' :: Parser s u s -> Parser s u a -> Parser s u [a]
sepBy' s p = sepBy s p <|> pure []


count :: Int -> Parser s u a -> Parser s u [a]
count 0 _ = return []
count n p = do
    x <- p
    xs <- count (n - 1) p
    return (x:xs)


skip :: Parser s u a -> Parser s u ()
skip p@(Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success ())
    (s', Success _) -> (unParser (skip p)) s')


skipn :: Int -> Parser s u a -> Parser s u ()
skipn 0 (Parser a) = Parser (\s -> let (s', _) = a s
    in (s', Success ()))
skipn n p@(Parser a) = Parser (\s -> case a s of
    (_, Failure _) -> (s, Success ())
    (_, Success _) -> (unParser (skipn (n - 1) p)) s)


parse :: (Stream s t) => Parser s u a -> s -> ParseState a
parse (Parser a) = snd . a


prefix :: Parser s u a -> Parser s u a -> Parser s u a
prefix s p = s *> p


maybePrefix :: Parser s u a -> Parser s u a -> Parser s u a
maybePrefix s p = try s *> p


suffix :: Parser s u a -> Parser s u a -> Parser s u a
suffix p s = p <* s


maybeSuffix :: Parser s u a -> Parser s u a -> Parser s u a
maybeSuffix p s = p <* try s


choose :: [Parser s u a] -> Parser s u a
choose ps = foldr (<|>) empty (fmap try ps)


wrapped :: Parser s u a -> Parser s u b -> Parser s u b
wrapped s p = s *> p <* s


between :: Parser s u a -> Parser s u b -> Parser s u c -> Parser s u b
between ls p rs = ls *> p <* rs
