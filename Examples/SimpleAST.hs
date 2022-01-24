module Examples.SimpleAST where

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


type Map = M.Map String Expr


type Parser' a = Parser T.Text Context a


data Type = Str | Int | Flt


data Expr
    = StrLit String
    | IntLit Integer
    | FltLit Double
    | Variable Type String
    -- | Lambda Expr
    | Loop {
        loopCond :: Expr,
        loopBody :: [Expr]
    }
    | Branch {
        branchCond :: Expr,
        branchBody :: [Expr]
    }
    | Assign {
        varName :: String,
        asnValue :: Expr
    }
    | Print {
        printVal :: Expr
    }
    | FunctionCall {
        funcName :: String,
        funcArgs :: [Expr]
    }


data Context = Context {
        ctxParams :: [Expr],
        ctxStack :: [Map]
    }


currStack :: Parser' Map
currStack = head . ctxStack <$> getState


stackPush :: Context -> Context
stackPush (Context ps st) = Context ps (M.empty:st)


alloc :: Expr -> Expr -> Context -> Context
alloc (Variable t n) e (Context ps stk) =
    Context ps ((M.insert n (t, e) (head stk)):tail stk)
alloc _ _ _ = error "cannot add non-var expr to stack"


addParam :: Expr -> Context -> Context
addParam p@(Variable _ _) (Context ps stk) =
    Context (p:ps) stk
addParam _ _ = error "cannot add non-var expr as param"


strLit :: Parser' Expr
strLit = StrLit <$> wrap (oneOf "'\"") (zeroOrMore anyChar)


intLit :: Parser' Expr
intLit = IntLit <$> integral


fltLit :: Parser' Expr
fltLit = FltLit <$> fractional


iden :: Parser' String
iden = ((:) <$> char '$' <*> oneOrMore alnum)
        <?> ((:) <$> validFirst <*> alnum)
        where
            validFirst = satisfy (\c -> isAlpha c || c == '_')


varDecl :: Parser' Expr
varDecl = strDecl <?> fltDecl <?> intDecl
    where
        strDecl = do
            _ <- lexeme $ string "String"
            Variable Str <$> iden
        fltDecl = do
            _ <- lexeme $ string "Float"
            Variable Flt <?> iden
        intDecl = do
            _ <- lexeme $ string "Integer"
            Variable Int <$> iden


var :: Parser' Expr
var = do
    i <- iden
    st <- currStack
    case lookup i st of
        (Nothing) -> fail "unknown variable"
        (Just v)  -> return v


-- Example: (Integer n, String str)<String>{ ... }
-- lambda :: Parser' Expr
-- lambda = while
    -- params <- parens sepBy' (char ',') varDecl
    -- _ <- skipws
    -- rtn <- angles iden
    -- _ <- braces body
    -- -- add params to context
    -- -- ...


while :: Parser' Expr
while = do
    _ <- string "while"
    c <- parens term
    b <- braces body <?> statement
    return $ Loop c b


branch :: Parser' Expr
branch = do
    _ <- string "if"
    c <- parens term
    b <- braces body
    return $ Branch c b


assign :: Parser' ()
assign = do
    i <- varDecl
    _ <- skipws
    _ <- string ":="
    _ <- skipws
    v <- term
    modifyState (alloc i v)


term :: Parser' Expr
term =  strLit
    <?> intLit
    <?> fltLit
    <?> iden
    -- <?> lambda


body :: Parser' [Expr]
body = zeroOrMore statement


statement :: Parser' Expr
statement = (while
        <?> branch
        <?> term)
        <* char ';'
