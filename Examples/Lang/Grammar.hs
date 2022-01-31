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


comment :: Parser' ()
comment = do
    _ <- newline
    choose [
        string "$$ " >> skipUntil newline,
        skipBtwn (string "$-") (string "-$")
        ]


space :: Parser' ()
space = comment <?> skip Harser.Char.space


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


-- | ex: pure foo<Int> { a<Int> | b<Int> } := add { a | b }
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


-- | ex: add { 1 | 2 }
funcCall :: Parser' Expr
funcCall = do
    nm <- iden
    _ <- skipws
    as <- braces (wrap skipws (char '|') `splits` value)
    (return $ FuncCall nm as) !> " | funcCall"
