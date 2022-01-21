module Harser.Combinators (
    Alternative(..),
    (<?>),
    zeroOrOne,
    zeroOrMore,
    oneOrMore,
    try,
    exactly,
    option,
    sepBy, sepBy',
    atLeast,
    atMost,
    count,
    skip,
    skips, skips',
    skipn,
    choose, choose',
    select, select',
    wrap,
    between
) where

import Control.Applicative (Alternative(..))

import Harser.Parser (Parser(..), ParseState(..), satisfy)
import Harser.Stream (Stream(..))


infixr 1 <?>
-- | like <|>, but with backtracking
(<?>) :: Parser s u a -> Parser s u a -> Parser s u a
(Parser lf) <?> (Parser rf) = Parser (\s -> case lf s of
        (_, Failure _) -> rf s
        (s', Success x) -> (s', Success x))


zeroOrOne :: Parser s u a -> Parser s u (Maybe a)
zeroOrOne (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success Nothing)
    (s', Success x) -> (s', Success (Just x)))


zeroOrMore :: Parser s u a -> Parser s u [a]
zeroOrMore = many


oneOrMore :: Parser s u a -> Parser s u [a]
oneOrMore = some


-- | restores stream to previous state on failure
try :: Parser s u a -> Parser s u a
try (Parser f) = Parser (\s -> case f s of
    (_, Failure e)  -> (s, Failure e)
    (s', Success x) -> (s', Success x))


exactly :: (Eq t, Stream s t) => t -> Parser s u t
exactly t = satisfy (== t)


-- | takes a parser and an default value. upon failure,
-- | succeeds with the default value.
option :: Parser s u a -> a -> Parser s u a
option (Parser a) d = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success d)
    (s', Success x) -> (s', Success x))


-- | 1+
sepBy :: Parser s u a -> Parser s u b -> Parser s u [b]
sepBy s p = fmap (:) p <*> zeroOrMore (s *> p)
    -- do
    -- x <- p
    -- xs <- zeroOrMore (s *> p)
    -- return (x:xs)


-- | 0+
sepBy' :: Parser s u a -> Parser s u b -> Parser s u [b]
sepBy' s p = sepBy s p <|> pure []


atLeast :: Int -> Parser s u a -> Parser s u [a]
atLeast 0 p = zeroOrMore p
atLeast 1 p = oneOrMore p
atLeast n p = do
    -- im sure there is an easier way, but it
    -- would look as bad as atMost
    firsts <- count n p
    rest' <- zeroOrMore p -- apostrophe bc shadow from Stream
    return (firsts ++ rest')


atMost :: Int -> Parser s u a -> Parser s u [a]
atMost 0 _ = pure []
atMost 1 p = fmap (:[]) p
atMost n p@(Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success [])
    (s', Success x) -> case (unParser nxt) s' of
        (s'', Failure _)  -> (s'', Success [x])
        (s'', Success xs) -> (s'', Success (x:xs)))
            where nxt = atMost (n - 1) p


count :: Int -> Parser s u a -> Parser s u [a]
count 0 _ = return []
count n p = do
    x <- p
    xs <- count (n - 1) p
    return (x:xs)


skip :: Parser s u a -> Parser s u ()
skip (Parser a) = Parser (\s -> case a s of
    (s', Failure e) -> (s', Failure e)
    (s', Success _) -> (s', Success ()))


-- | 0+
skips :: Parser s u a -> Parser s u ()
skips p = zeroOrMore p >> return ()


-- | 1+
skips' :: Parser s u a -> Parser s u ()
skips' p = oneOrMore p >> return ()


skipn :: Int -> Parser s u a -> Parser s u ()
skipn n p = count n p >> return ()


choose :: (Foldable l) => l (Parser s u a) -> Parser s u a
choose ps = foldr (<|>) empty ps


-- | like choose, but uses <?> instead of <|>
choose' :: (Foldable l) => l (Parser s u a) -> Parser s u a
choose' ps = foldr (<?>) empty ps


select :: (Functor l, Foldable l) =>
    (a -> Parser s u a) -> l a -> Parser s u a
select fp l = foldr (<|>) empty as
    where as = fmap fp l


select' :: (Functor l, Foldable l) =>
    (a -> Parser s u a) -> l a -> Parser s u a
select' fp l = foldr (<?>) empty as
    where as = fmap fp l


wrap :: Parser s u a -> Parser s u b -> Parser s u b
wrap s p = s *> p <* s


between :: Parser s u a -> Parser s u b -> Parser s u c -> Parser s u b
between ls p rs = ls *> p <* rs



