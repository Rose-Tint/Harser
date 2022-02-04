module Examples.Lang.Lang where

import Prelude hiding (fail)

import Control.Monad hiding (fail)

import Harser.Combinators
import Harser.Parser

import Examples.Lang.Data
import Examples.Lang.Grammar
import Examples.Lang.State


lexer :: Parser' Expr
lexer = funcDef <?> term


-- instead of substitution, we can add the arguments
-- to the stack with the same name as their
-- respective parameter. Then in the function,
-- the parameters just get their value from the
-- stack.
callFunc :: Function -> [Value] -> Parser' Value
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


runExpr :: Expr -> Parser' Value
runExpr (FuncCall nm as) = do
    fn <- findFunc nm
    callFunc fn as !> " | runExpr"
runExpr (FuncDef _) = fail "| cannot define function here"
runExpr (ValueExpr v) = return v
