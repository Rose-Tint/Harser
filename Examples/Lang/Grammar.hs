module Examples.Lang.Grammar where

import Prelude hiding (lookup, fail)

import Harser.Char hiding (skipws,
                           space,
                           spaces)
import qualified Harser.Char (space)
import Harser.Combinators
import Harser.Parser hiding (State)
import Harser.Utilities hiding (lexeme)

import Examples.Lang.Data
import Examples.Lang.State


space :: Parser' ()
space = choose [
        (string "$$ " >> skipUntil newline),
        (skipBtwn (lexeme $ string "$-") (string "-$")),
        (skip Harser.Char.space)
    ]


-- | skips whitespace, inline comments, and block comments
skipws :: Parser' ()
skipws = zeroOrMore space >> pure ()


spaces :: Parser' ()
spaces = space >> skipws


lexeme :: Parser' a -> Parser' a
lexeme p = (p <* spaces) !> " | lexeme"


-- | ex: foo
iden :: Parser' String
iden = ((:) <$> letter <*> zeroOrMore alnum)
    !> " | iden"


value :: Parser' Value
value = choose [
        (IntVal <$> integral),
        (FltVal <$> fractional),
        (StrVal <$> wrap (char '"') (zeroOrMore anyChar)),
        (VarVal <$> iden)
    ] !> " | value"


purity :: Parser' Bool
purity = do
    pStr <- select string ["pure", "impure"]
    case pStr of
        "pure"   -> return True
        "impure" -> return False
        _        -> fail " | purity"


paramList :: Parser' [Var]
paramList = splits delim param where
    delim = wrap skipws (char '|')
    param = do
        nm <- iden
        tn <- wrap skipws (angles (wrap skipws iden))
        tp <- findType tn
        return $ Par nm tp


-- | ex: pure sub<Int> { a<Int> | b<Int> } := add a (0 - b)
funcDef :: Parser' Expr
funcDef = do
    ip <- lexeme purity
    nm <- iden
    _ <- skipws
    tn <- angles (wrap skipws iden) !> " | in params"
    tp <- findType tn
    _ <- skipws
    ps <- braces paramList
    _ <- wrap skipws (string ":=")
    bd <- funcCall -- (if ip then funcCall else pureFnCall)
        !> " | in body"
    _ <- allocFunc nm ps tp bd ip !> " | in allocation"
    return $ FuncDef nm


pureFnCall :: Parser' Expr
pureFnCall = do
    nm <- iden
    fn <- findFunc nm
    if not $ isPure fn then
        fail " | pureFnCall"
    else
        let par_c = length (fnParams fn) in do
            as <- count par_c getValue
            return $ FuncCall nm as
        where
            getValue = do
                _ <- skipws >> space
                v <- iden >>= findVar
                return $ varValue v


funcCall :: Parser' Expr
funcCall = do
    nm <- lexeme iden
    fn <- findFunc nm
    let par_c = length (fnParams fn)
    as <- count par_c (value <* space)
    (return $ FuncCall nm as) !> " | funcCall"


pureStmnt :: Parser' Expr
pureStmnt = pureFnCall
