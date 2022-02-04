{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Harser.Utilities
Description : Helpful parser utilities
-}

module Harser.Utilities where

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Stream


-- |@'lexeme' p@ parses @p@, and then one or more
-- whitespace characters (using 'spaces'), and
-- returns the result of p
lexeme :: (Stream s Char) => Parser s u a -> Parser s u a
lexeme p = p <* spaces


-- |Parses a number with a decimal point.
fractional :: (Stream s Char, Fractional n, Read n)
           => Parser s u n
fractional = do
    whole <- oneOrMore digit
    dot <- char '.'
    dec <- oneOrMore digit
    return $ read (whole ++ (dot:dec))


-- |Parses a whole number
integral :: (Stream s Char, Integral n, Read n)
         => Parser s u n
integral = read <$> oneOrMore digit


-- |@'parens' p@ parses @p@ surrounded by parentheses
parens :: (Stream s Char) => Parser s u a -> Parser s u a
parens p = between (char '(') p (char ')')


-- |This was a bad idea. Remove ASAP
{-# DEPRECATED parens' "Use 'parens' instead" #-}
parens' :: (Stream s Char) => Parser s u a -> Parser s u a
parens' p = between (char '(') (wrap skipws p) (char ')')


-- |@'braces' p@ parses @p@ surrounded by braces
braces :: (Stream s Char) => Parser s u a -> Parser s u a
braces p = between (char '{') p (char '}')


-- |This was a bad idea. Remove ASAP
{-# DEPRECATED braces' "Use 'braces' instead" #-}
braces' :: (Stream s Char) => Parser s u a -> Parser s u a
braces' p = between (char '{') (wrap skipws p) (char '}')


-- |@'brackets' p@ parses @p@ surrounded by brackets
brackets :: (Stream s Char) => Parser s u a -> Parser s u a
brackets p = between (char '[') p (char ']')


-- |This was a bad idea. Remove ASAP
{-# DEPRECATED brackets' "Use 'brackets' instead" #-}
brackets' :: (Stream s Char) => Parser s u a -> Parser s u a
brackets' p = between (char '[') (wrap skipws p) (char ']')


-- |@'angles' p@ parses @p@ surrounded by angles
angles :: (Stream s Char) => Parser s u a -> Parser s u a
angles p = between (char '<') p (char '>')


-- |This was a bad idea. Remove ASAP
{-# DEPRECATED angles' "Use 'angles' instead" #-}
angles' :: (Stream s Char) => Parser s u a -> Parser s u a
angles' p = between (char '<') (wrap skipws p) (char '>')
