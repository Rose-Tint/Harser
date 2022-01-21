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

import Harser.Parser (Parser(..), ParseState(..), satisfy, runP)
import Harser.Stream (Stream(..))


infixr 1 <?>
-- | like <|>, but with backtracking
(<?>) :: Parser s u a -> Parser s u a -> Parser s u a
(Parser lf) <?> (Parser rf) = Parser (\s -> case lf s of
        (_, Failure _)  -> rf s
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
sepBy s p = (:) <$> p <*> zeroOrMore (s *> p)


-- | 0+
sepBy' :: Parser s u a -> Parser s u b -> Parser s u [b]
sepBy' s p = sepBy s p <?> pure []


atLeast :: Int -> Parser s u a -> Parser s u [a]
atLeast 0 p = zeroOrMore p
atLeast 1 p = oneOrMore p
atLeast n p = (++) <$> (count n p) <*> zeroOrMore p


atMost :: Int -> Parser s u a -> Parser s u [a]
atMost 0 _ = pure []
atMost 1 p = fmap (:[]) p
atMost n p@(Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success [])
    (s', Success x) -> case runP (atMost (n - 1) p) s' of
        (s'', Failure _)  -> (s'', Success [x])
        (s'', Success xs) -> (s'', Success (x:xs)))


count :: Int -> Parser s u a -> Parser s u [a]
count 0 _ = return []
count n p = (:) <$> p <*> (count (n - 1) p)


skip :: Parser s u a -> Parser s u ()
skip (Parser a) = Parser (\s -> case a s of
    (s', Failure e) -> (s', Failure e)
    (s', Success _) -> (s', Success ()))


-- | 0+
skips :: Parser s u a -> Parser s u ()
skips p = zeroOrMore p >> pure ()


-- | 1+
skips' :: Parser s u a -> Parser s u ()
skips' p = oneOrMore p >> pure ()


skipn :: Int -> Parser s u a -> Parser s u ()
skipn n p = count n p >> pure ()


choose :: [Parser s u a] -> Parser s u a
choose [] = fail "choose"
choose (p:ps) = foldr (<?>) p ps


-- | choose without backtracking
choose' :: [Parser s u a] -> Parser s u a
choose' [] = fail "choose'"
choose' (p:ps) = foldr (<|>) p ps


select :: (a -> Parser s u a) -> [a] -> Parser s u a
select _ [] = fail "select"
select p (x:xs) = foldr (<?>) (p x) (fmap p xs)


-- | select without backtracking
select' :: (a -> Parser s u a) -> [a] -> Parser s u a
select' _ [] = fail "select'"
select' p (x:xs) = foldr (<|>) (p x) (fmap p xs)


wrap :: Parser s u a -> Parser s u b -> Parser s u b
wrap s p = s *> p <* s


between :: Parser s u a -> Parser s u b -> Parser s u c -> Parser s u b
between ls p rs = ls *> p <* rs



