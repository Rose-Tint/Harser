module Harser.State (
    ParseError,
    StreamPos(..),
    State(..),
    StreamPosAccessor(..)
) where


type ParseError = String


-- |Represents the current position in the input
-- stream for debugging purposes
data StreamPos
    = StreamPos {
        getSrcLn  :: Int,
        getSrcCol :: Int
    }


-- |@'State' s u@ represents the current state of
-- a @'Parser'@ with stream type @s@ and user type
-- @u@
data State s u
    = State {
        getStatePos    :: StreamPos,
        getStateStream :: s,
        getStateUser   :: !u
    }


class StreamPosAccessor a where
    -- |Returns the current StreamPos value
    getStreamPos :: a -> StreamPos
    -- |Returns the current line number
    getLinePos :: a -> Int
    getLinePos a = getLinePos $ getStreamPos a
    -- |Returns the current column number
    getColPos :: a -> Int
    getColPos a = getColPos $ getStreamPos a
    -- |Increments the current line number
    incLine :: a -> a
    -- |Increments the current column number
    incCol :: a -> a


instance StreamPosAccessor StreamPos where
    getStreamPos = id
    getLinePos (StreamPos ln _) = ln
    getColPos (StreamPos _ col) = col
    incLine (StreamPos ln col) = StreamPos (ln + 1) col
    incCol (StreamPos ln col) = StreamPos ln (col + 1)


instance StreamPosAccessor (State s u) where
    getStreamPos = getStatePos
    incLine (State p s u) = State (incLine p) s u
    incCol (State p s u) = State (incCol p) s u
