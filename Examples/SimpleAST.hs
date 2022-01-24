module Examples.SimpleAST where

import Data.Char
-- import qualified Data.Text as T
import qualified Data.Map as M

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


-- 'Simple' interpreted functional language


type Map = M.Map String Expr

type Parser' a = Parser String Map a


data Value
    = Str String
    | Int Integer
    | Flt Double


data Expr
    = Function {
        fnName    :: String,
        fnRtnType :: String,
        fnParams  :: [(String, String)],
        fnBody    :: [Expr] -> Value
    }
    | Val Value
    | IfElse {
        ieCond    :: Expr,
        iBody     :: Expr,
        eBody     :: Expr
    }


addFunction :: Expr -> Parser' ()
addFunction f@(Function nm _ _ _) = modifyState (M.insert nm f)
addFunction _ = error "non-Function used as arg in addFunction"


value :: Parser' Value
value = (Str <$> wrap (char '"') (zeroOrMore anyChar))
    <?> (Int <$> integral)
    <?> (Flt <$> fractional)


iden :: Parser' String
iden = (:) <$> letter <*> zeroOrMore (alnum <?> char '_')


-- | example:
-- fn foo: Int <bar: String | baz: Flt> := ...
fnDef :: Parser' ()
fnDef = do
    _ <- lexeme $ string "fn"
    name <- iden
    _ <- wrap skipws (char ':')
    rtn <- iden
    _ <- skipws
    ps <- angles (sepBy (wrap skipws (char '|')) param)
    _ <- wrap skipws (string ":=")
    bdy <- body
    addFunction $ Function name rtn ps bdy
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


fnCall :: Parser' Expr
fnCall = do
    st <- getState
    nm <- iden
    _ <- wrap skipws (char '%')
    as <- sepBy space iden
    case lookup nm st of
        (Nothing) -> fail "function not found"
        (Just e)  -> case e of
            (Function _ rtn ps bdy) -> do
                if length ps /= length as then
                    fail "args do not match params"
                else bdy as
            _ -> fail $ nm ++ " is not a function"


run :: IO ()
run = hSetEcho stdin False >> (loop M.empty)


loop :: Map -> IO ()
loop mp = inlnPrompt "~>>" >>= (\inp -> case inp of
    "exit"  -> exitSuccess
    "stop"  -> return ()
    "clear" -> loop M.empty
    _       -> case parse (fnDef <?> fnCall) inp mp of
        (Failure e)      ->
            (putStrLn $ "!>> " ++ e) >> loop mp
        (Success (m, e)) ->
            (putStrLn $ "=>> " ++ show (eval m e)) >> loop m)
