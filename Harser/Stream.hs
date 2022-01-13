{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Harser.Stream where

import qualified Data.Text as T (Text, uncons)


-- | stream type s | token type t
class Stream s t | s -> t where
    uncons :: s -> Maybe (t, s)
    next :: s -> Maybe t
    next = fmap fst . uncons
    rest :: s -> Maybe s
    rest = fmap snd . uncons


instance Stream [t] t where
    uncons [] = Nothing
    uncons (t:ts) = Just (t, ts)


instance Stream T.Text Char where
    uncons = T.uncons
