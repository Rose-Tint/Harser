module Harser.Combinators where

import Control.Applicative

import Harser.Parser


(<?>) :: Parser s a -> Parser s a -> Parser s a
lp <?> rp = try lp <|> rp
infixl 7 <?>


zeroOrOne :: Parser s a -> Parser s (Maybe a)
zeroOrOne (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success Nothing)
    (s', Success a') -> (s', Success (Just a')))


zeroOrMore :: Parser s a -> Parser s [a]
zeroOrMore (Parser f) = Parser go where
    go s = case f s of
        (_, Failure _)  -> (s, Success [])
        (s', Success a) -> case go s' of 
            (s'', Failure e) -> (s'', Failure e)
            (s'', Success as) -> (s'', Success (a:as))


oneOrMore :: Parser s a -> Parser s [a]
oneOrMore (Parser f) = Parser (\s -> case f s of
    (s', Failure e) -> (s', Failure e)
    (s', Success a) -> let (Parser f') = zeroOrMore (Parser f)
        in case f' s' of
            (s'', Failure e)  -> (s'', Failure e)
            (s'', Success as) -> (s'', Success (a:as)))


option :: Parser s a -> a -> Parser s a
option (Parser a) o = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success o)
    (s', Success x) -> (s', Success x))


optional :: Parser s a -> Parser s ()
optional (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success ())
    (s', Success _) -> (s', Success ()))


-- | 1+
sepBy :: Parser s a -> Parser s b -> Parser s [b]
sepBy s p = do
    x <- p
    xs <- zeroOrMore (s *> p)
    return (x:xs)


-- | 0+
sepBy' :: Parser s a -> Parser s b -> Parser s [b]
sepBy' s p = sepBy s p <|> pure []


count :: Int -> Parser s a -> Parser s [a]
count 0 _ = return []
count n p = do
    x <- p
    xs <- count (n - 1) p
    return (x:xs)


skip :: Parser s a -> Parser s ()
skip p@(Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success ())
    (s', Success _) -> (unParser (skip p)) s')


skipn :: Int -> Parser s a -> Parser s ()
skipn 0 (Parser a) = Parser (\s -> let (s', _) = a s
    in (s', Success ()))
skipn n p@(Parser a) = Parser (\s -> case a s of
    (_, Failure _) -> (s, Success ())
    (_, Success _) -> (unParser (skipn (n - 1) p)) s)


choose :: [Parser s a] -> Parser s a
choose ps = foldr (<?>) empty (fmap try ps)


wrapped :: Parser s a -> Parser s b -> Parser s b
wrapped s p = s *> p <* s


between :: Parser s a -> Parser s b -> Parser s c -> Parser s b
between ls p rs = ls *> p <* rs
