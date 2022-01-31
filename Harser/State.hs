module Harser.State where


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
