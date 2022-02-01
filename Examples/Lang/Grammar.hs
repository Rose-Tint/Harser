module Examples.Lang.Grammar where

import Prelude hiding (lookup, fail)

import Harser.Combinators
import Harser.Parser hiding (State)

import Examples.Lang.Data
import Examples.Lang.State
import Examples.Lang.Lexer


iden :: Parser' String
iden = getNext >>= \t -> case t of
    IdenTok i -> return i
    _         -> fail "iden"


asnOper :: Parser' ()
asnOper = exactly AssignmentTok >> return ()


rtnType :: Parser' Type
rtnType = do
    _ <- exactly RtnTypeDeclTok
    tn <- iden
    tp <- findType tn
    return tp


comma :: Parser' ()
comma = exactly CommaTok >> return ()


colon :: Parser' ()
colon = exactly ColonTok >> return ()


value :: Parser' Value
value = getNext >>= \t -> case t of
        IntTok n    -> return $ IntVal n
        FloatTok n  -> return $ FltVal n
        StringTok s -> return $ StrVal s
        IdenTok i   -> return $ VarVal i
        _           -> fail "value"


parens :: Parser' a -> Parser' a
parens p = between (exactly LParenTok) p (exactly RParenTok)
    !> "parens"


term :: Parser' Expr
term = choose [
        parens term,
        funcCall,
        ValueExpr <$> value
    ] !> "term"


purity :: Parser' Bool
purity = getNext >>= \t -> case t of
    KwrdTok Pure   -> return True
    KwrdTok Impure -> return False
    _                 -> fail "purity"


paramList :: Parser' [Var]
paramList = comma `splits` param
    where
        param = do
            nm <- iden
            _ <- colon
            tp <- findType =<< iden
            return $ Par nm tp


-- | ex: pure foo(a: Int, b: Int) => Int := add(a, b)
funcDef :: Parser' Expr
funcDef = do
    ip <- purity
    nm <- iden
    ps <- parens paramList
    tp <- rtnType
    _ <- asnOper
    bd <- term
    _ <- allocFunc nm ps tp bd ip !> "in allocation"
    return $ FuncDef nm


-- | ex: add(1, 2)
funcCall :: Parser' Expr
funcCall = do
    nm <- iden
    as <- parens $ comma `splits` value
    return (FuncCall nm as) !> "funcCall"
