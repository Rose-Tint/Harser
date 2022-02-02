{-# LANGUAGE FlexibleContexts #-}

module Harser.Utilities where

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Stream


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


parens :: (Stream s Char) => Parser s u a -> Parser s u a
parens p = between (char '(') p (char ')')


parens' :: (Stream s Char) => Parser s u a -> Parser s u a
parens' p = between (char '(') (wrap skipws p) (char ')')


braces :: (Stream s Char) => Parser s u a -> Parser s u a
braces p = between (char '{') p (char '}')


braces' :: (Stream s Char) => Parser s u a -> Parser s u a
braces' p = between (char '{') (wrap skipws p) (char '}')


brackets :: (Stream s Char) => Parser s u a -> Parser s u a
brackets p = between (char '[') p (char ']')


brackets' :: (Stream s Char) => Parser s u a -> Parser s u a
brackets' p = between (char '[') (wrap skipws p) (char ']')


angles :: (Stream s Char) => Parser s u a -> Parser s u a
angles p = between (char '<') p (char '>')


angles' :: (Stream s Char) => Parser s u a -> Parser s u a
angles' p = between (char '<') (wrap skipws p) (char '>')
