module Examples.SimpleAST where

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


-- 'Simple' interpreted functional language


type Map = M.Map String Expr

type Parser' a = Parser T.Text Map a


data Expr
    = Function {
        fnName :: String,
        fnRtnType :: String,
        fnParams :: [(String, String)],
        fnBody :: Expr
    }
    | IfElse {
        ieCond :: Expr,
        iBody  :: Expr,
        eBody  :: Expr
    }


addFunction :: Expr -> Parser' ()
addFunction f@(Function nm _ _ _) = modifyState (M.insert nm f)
addFunction _ = error "non-Function used as arg in addFunction"


iden :: Parser' String
iden = (:) <$> letter <*> zeroOrMore (alnum <?> char '_')


-- | example:
-- fn foo: Int <bar: String | baz: Flt> := ...
function :: Parser' ()
function = do
    _ <- lexeme $ string "fn"
    name <- iden
    _ <- wrap skipws (char ':')
    rtn <- iden
    _ <- skipws
    ps <- angles (sepBy (wrap skipws (char '|')) param)
    _ <- wrap skipws (string ":=")
    bdy <- body
    addFunction $  Function name rtn ps bdy
        where
            paramDecl = do
                i <- iden
                _ <- wrap skipws (char ':')
                t <- iden
                return (i, t)


ifElse :: Parser' Expr
ifElse = do
    _ <- string "if"
    cond <- wrap skipws (parens expr)
    iBdy <- body
    _ <- spaces
    _ <- lexeme $ string "else"
    eBdy <- body
    return $ IfElse cond iBdy eBdy
