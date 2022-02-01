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


data StreamPos = StreamPos {
    getSrcLn  :: Int,
    getSrcCol :: Int
}


data State s u
    = State {
        getStatePos    :: StreamPos,
        getStateStream :: s,
        getStateUser   :: !u
    }


getStateLn :: State s u -> Int
getStateLn (State p _ _) = getSrcLn p


getStateCol :: State s u -> Int
getStateCol (State p _ _) = getSrcCol p


incStateLn :: State s u -> State s u
incStateLn (State p s u) = State (incSrcLn p) s u


incStateCol :: State s u -> State s u
incStateCol (State p s u) = State (incSrcCol p) s u


incSrcLn :: StreamPos -> StreamPos
incSrcLn (StreamPos ln col) = StreamPos (ln + 1) col


incSrcCol :: StreamPos -> StreamPos
incSrcCol (StreamPos ln col) = StreamPos ln (col + 1)


class StreamPosAccessor a where
    getStreamPos :: a -> StreamPos
    getLinePos :: a -> Int
    getLinePos a = getLinePos $ getStreamPos a
    getColPos :: a -> Int
    getColPos a = getColPos $ getStreamPos a
    incLine :: a -> a
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
