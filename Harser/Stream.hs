{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Harser.Stream (
    Stream(..)
) where

import qualified Data.Text as T (Text, uncons)


-- |@'Stream' s t@ has underlying stream type @s@
-- and token type @t@
class (Show t) => Stream s t | s -> t where
    -- |Returns @'Nothing'@ if the stream is empty;
    -- otherwise returns @'Just' (t, s)@ where @t@
    -- is the next token, and @s@ is the rest of
    -- the stream.
    uncons :: s -> Maybe (t, s)


instance (Show t) => Stream [t] t where
    uncons [] = Nothing
    uncons (t:ts) = Just (t, ts)


instance Stream T.Text Char where
    uncons = T.uncons
