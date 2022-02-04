{-|
Module      : Harser.Combinators
Description : Helpful parser combinators

This module contains many useful and often integral
parser combinators.
-}

module Harser.Combinators (
    Alternative(..),
    (<?>),
    zeroOrOne,
    zeroOrMore,
    oneOrMore,
    try,
    exactly,
    option,
    splits, splits',
    splitn,
    atLeast,
    atMost,
    count,
    skip,
    skips, skips',
    skipn,
    skipUntil,
    skipBtwn,
    choose, choose',
    select, select',
    maybeP,
    boolP,
    wrap,
    between
) where

import Control.Applicative (Alternative(..))

import Harser.Parser (
        Parser(..),
        ParseState(..),
        satisfy,
        runP,
        (<?>)
    )
import Harser.Stream (Stream(..))


-- |Returns @'Nothing'@ upon failure, and @'Just' r@
-- (where @r@ is the parsed result) upon success.
zeroOrOne :: Parser s u a -> Parser s u (Maybe a)
zeroOrOne (Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success Nothing)
    (s', Success x) -> (s', Success (Just x)))


-- |@'zeroOrMore' p@ parses zero or more instances
-- of @p@
zeroOrMore :: Parser s u a -> Parser s u [a]
zeroOrMore = many


-- |@'zeroOrMore' p@ parses one or more instances
-- of @p@
oneOrMore :: Parser s u a -> Parser s u [a]
oneOrMore = some


-- |If the given parser fails, resets the current
-- state. Does not consume input on failure.
try :: Parser s u a -> Parser s u a
try (Parser f) = Parser (\s -> case f s of
    (_, Failure e)  -> (s, Failure e)
    (s', Success x) -> (s', Success x))


-- |@'exactly' t@ is equivalent to @'satisfy'
-- (== t)@
exactly :: (Eq t, Stream s t) => t -> Parser s u t
exactly t = satisfy (== t)


-- |Takes a parser and a default value. Upon failure,
-- succeeds with the default value.
option :: Parser s u a -> a -> Parser s u a
option (Parser a) d = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success d)
    (s', Success x) -> (s', Success x))


-- |@delim `'splits'` psr@ parses one or more
-- @psr@'s, seperated by @delim@. Typically used
-- in infix for readability purposes.
splits :: Parser s u a -> Parser s u b -> Parser s u [b]
splits s p = (:) <$> p <*> zeroOrMore (s *> p)


-- |Same as 'splits', but parses zero or more.
splits' :: Parser s u a -> Parser s u b -> Parser s u [b]
splits' s p = splits s p <?> pure []


-- |@'splitn' n s p@ parses @p@, seperated by @s@
-- @n@ times
splitn :: Int -> Parser s u a -> Parser s u b -> Parser s u [b]
splitn 0 _ _ = pure []
splitn n s p = (:) <$> p <*> count (n - 1) (s *> p)


-- |@'atLeast' n p@ parses @n@ or more instances of
-- @p@
atLeast :: Int -> Parser s u a -> Parser s u [a]
atLeast 0 p = zeroOrMore p
atLeast 1 p = oneOrMore p
atLeast n p = (++) <$> (count n p) <*> zeroOrMore p


-- |@'atMost' n p@ parses at most @n@ instances of @p@
atMost :: Int -> Parser s u a -> Parser s u [a]
atMost 0 _ = pure []
atMost 1 p = fmap (:[]) p
atMost n p@(Parser a) = Parser (\s -> case a s of
    (s', Failure _) -> (s', Success [])
    (s', Success x) -> case runP (atMost (n - 1) p) s' of
        (s'', Failure _)  -> (s'', Success [x])
        (s'', Success xs) -> (s'', Success (x:xs)))


-- |@'count' n p@ parses exactly @n@ instances of @p@
count :: Int -> Parser s u a -> Parser s u [a]
count 0 _ = return []
count n p = (:) <$> p <*> (count (n - 1) p)


-- |@'skip' p@ parses @p@ (consuming input), and
-- returns @()@
skip :: Parser s u a -> Parser s u ()
skip (Parser a) = Parser (\s -> case a s of
    (s', Failure e) -> (s', Failure e)
    (s', Success _) -> (s', pure ()))


-- |@'skips' p@ runs p one or more times, then
-- returns @()@
skips :: Parser s u a -> Parser s u ()
skips p = oneOrMore p >> pure ()


-- |@'skips' p@ runs p zero or more times, then
-- returns @()@
skips' :: Parser s u a -> Parser s u ()
skips' p = zeroOrMore p >> pure ()


-- |@'skipn' n p@ skips exactly @n@ instances of
-- @p@
skipn :: Int -> Parser s u a -> Parser s u ()
skipn n p = count n p >> pure ()


-- |@'skipUntil' p@ runs p until p fails.
skipUntil :: Parser s u a -> Parser s u ()
skipUntil p = Parser $ \s -> case runP p s of
        (_, Failure _)  -> (s, pure ())
        (s', Success _) -> runP (skipUntil p) s'


-- |@'skipBtwn' a b@ parses @a@, and then skips until
-- @b@ fails.
skipBtwn :: Parser s u a -> Parser s u b -> Parser s u ()
skipBtwn a b = skip a >> skipUntil b


-- |Returns the result of the first successful parser
choose :: (Foldable t) => t (Parser s u a) -> Parser s u a
choose ps = if null ps then
        fail "choose"
    else
        foldl1 (<?>) ps


-- |'choose' without backtracking
choose' :: (Foldable t) => t (Parser s u a) -> Parser s u a
choose' ps = if null ps then
        fail "choose"
    else
        foldl1 (<|>) ps


-- |@'select' f l@ maps f to l, and then returns the
-- result of the first successful parser
select :: (Foldable t, Functor t)
        => (a -> Parser s u b) -> t a -> Parser s u b
select f ps = if null ps then
        fail "choose"
    else
        foldl1 (<?>) (fmap f ps)


-- |'select' without backtracking
select' :: (Foldable t, Functor t)
        => (a -> Parser s u b) -> t a -> Parser s u b
select' f ps = if null ps then
        fail "choose"
    else
        foldl1 (<|>) (fmap f ps)


-- |@'boolP' fp tp bp@ runs @fp@ if @bp@ fails, and
-- @tp@ if @bp@ succeeds. ignores the result of @bp@
boolP :: Parser s u a -> Parser s u a
      -> Parser s u b -> Parser s u a
boolP falseP trueP testP = Parser $ \s ->
    case runP testP s of
        (s', Failure e) -> runP (falseP !> e) s'
        (s', Success _) -> runP trueP s'


-- |@'wrap' s p@ is effectively the same as @'between'
-- s p s@
wrap :: Parser s u a -> Parser s u b -> Parser s u b
wrap s p = s *> p <* s


-- |@'between' lp p rp@ skips @lp@, runs @p@, skips
-- @rp@, and returns the result of @p@.
between :: Parser s u a -> Parser s u b
        -> Parser s u c -> Parser s u b
between lp p rp = lp *> p <* rp



