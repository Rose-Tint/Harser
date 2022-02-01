module Harser.State (
    ParseError,
    StreamPos(..),
    State(..),
    StreamPosAccessor(..),
    getStateLn,
    getStateCol,
    incStateLn,
    incStateCol,
    incSrcLn,
    incSrcCol
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


{-# DEPRECATED "Use StreamPosAccessor.getLinePos instead" #-}
getStateLn :: State s u -> Int
getStateLn (State p _ _) = getSrcLn p


{-# DEPRECATED "Use StreamPosAccessor.getColPos instead" #-}
getStateCol :: State s u -> Int
getStateCol (State p _ _) = getSrcCol p


{-# DEPRECATED "Use StreamPosAccessor.incLine instead" #-}
incStateLn :: State s u -> State s u
incStateLn (State p s u) = State (incSrcLn p) s u


{-# DEPRECATED "Use StreamPosAccessor.incCol instead" #-}
incStateCol :: State s u -> State s u
incStateCol (State p s u) = State (incSrcCol p) s u


{-# DEPRECATED "Use StreamPosAccessor.incLine instead" #-}
incSrcLn :: StreamPos -> StreamPos
incSrcLn (StreamPos ln col) = StreamPos (ln + 1) col


{-# DEPRECATED "Use StreamPosAccessor.incCol instead" #-}
incSrcCol :: StreamPos -> StreamPos
incSrcCol (StreamPos ln col) = StreamPos ln (col + 1)


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
