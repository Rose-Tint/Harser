module Examples.Lang.Lang where

import Prelude hiding (lookup, fail)

import System.IO (hSetEcho, hFlush, stdout, stdin)
import System.Exit (exitSuccess)
import Data.Map (fromList)
import Control.Monad hiding (fail)

import Harser.Combinators
import Harser.Parser hiding (State)

import Examples.Lang.Data
import Examples.Lang.Grammar
import Examples.Lang.State


-- evalExpr :: Expr -> Parser' ()
-- evalExpr e = case e of
--     (FuncCall nm as) -> do
--         fn <- findFunc nm
--         _ <- foldlM


lexer :: Parser' AST
lexer = do
    e <- funcCall <?> funcDef
    s <- getState
    return $ AST s e


-- instead of substitution, we can add the arguments
-- to the stack with the same name as their
-- respective parameter. Then in the function,
-- the parameters just get their value from the
-- stack.
callFunc :: Function -> [Value] -> Parser' ()
-- there has GOT to be a better way than "foldM ()"
callFunc (Function _ ps _ bd _) as = do 
    foldM setParam () (zip ps as)
    runExpr bd
    where
        setParam :: () -> (Var, Value) -> Parser' ()
        setParam () (par, val) = do
            valT <- valType val
            if varType par /= valT then
                fail " | arg type does not match par type"
            else do
                alloc $ Var (varName par) (varType par) val


runExpr :: Expr -> Parser' ()
runExpr (FuncCall nm as) = do
    fn <- findFunc nm
    callFunc fn as !> " | runExpr"
runExpr (FuncDef _) = fail "| cannot define function here"


stdlib :: State
stdlib = State {
        stack = [fromList [
            (func "add"
                [Par "a" Integer, Par "b" Integer]
                Integer
                (FuncCall "" [IntVal 0, IntVal 0]))
        ]],
        types = [fromList [
            rec "Int"
        ]]
    }
        where
            rec nm = (nm, Record nm [])
            func nm ps rtn bd =
                (nm, Var nm FuncType (FuncVal $
                    Function nm ps rtn bd True))


inlnPrompt :: String -> IO String
inlnPrompt p = do
    _ <- putStr p
    _ <- hFlush stdout
    getLine


run :: IO ()
run = hSetEcho stdin False >> (loop stdlib)


loop :: State -> IO ()
loop st = inlnPrompt ">>>" >>= (\inp -> case inp of
    "exit"  -> exitSuccess
    "stop"  -> return ()
    "stack" -> do
        putStrLn $ "?>> " ++ show st
        loop st
    "clear" -> loop stdlib
    _       -> case parse'' lexer inp st of
        (st', Failure e)        -> do
            let col = getStateCol st'
            _ <- putStr "~>> "
            _ <- putStr $ replicate (col - 1) '~'
            _ <- putChar '^'
            _ <- putStrLn $ replicate (length inp - col - 1) '~'
            _ <- putStrLn $ "!>> error: " ++ e
            loop $ getStateUser st'
        (_, Success (AST m e)) -> do
            case e of
                _       -> putStrLn $ "=>> " ++ show e
            loop m)
