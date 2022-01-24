module Examples.SimpleAST where

import qualified Data.Map as M
import System.IO
import System.Exit

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


-- 'Simple' interpreted functional language


type Map = M.Map String Expr

type Parser' a = Parser String Map a


data Value
    = Void
    | Str String
    | Int Integer
    | Flt Double
    deriving (Show, Eq, Ord)


data Expr
    = Function {
        fnRtnType :: String,
        fnParams  :: [(String, String)],
        fnBody    :: [Value] -> Value
    }
    | Var String
    | Val Value


value :: Parser' Value
value = (Str <$> wrap (char '"') (zeroOrMore anyChar))
    <?> (Flt <$> fractional)
    <?> (Int <$> integral)


iden :: Parser' String
iden = (:) <$> letter <*> zeroOrMore (alnum <?> char '_')


-- | example:
-- fn foo: Int <bar: String | baz: Flt> := ...
fnDef :: Parser' Expr
fnDef = do
    _ <- lexeme $ string "fn"
    name <- iden
    _ <- wrap skipws (char ':')
    rtn <- iden
    _ <- skipws
    ps <- angles $ sepBy (wrap skipws (char '|')) param
    _ <- wrap skipws (string ":=")
    let fn = Function rtn ps (\_ -> Void)
    _ <- modifyState (M.insert name fn)
    return fn
        where
            param = do
                i <- iden
                _ <- wrap skipws (char ':')
                t <- iden
                return (i, t)


varDef :: Parser' Expr
varDef = do
    _ <- lexeme $ string "var"
    nm <- iden
    _ <- wrap skipws (char ':')
    _ <- iden
    _ <- wrap skipws (string ":=")
    v <- value
    let val = Val v
    _ <- modifyState (M.insert nm val)
    return val


fnCall :: Parser' Expr
fnCall = do
    st <- getState
    nm <- iden
    _ <- wrap skipws (char '%')
    as <- fmap (fmap Var) (sepBy space iden)
    case M.lookup nm st of
        (Nothing) -> fail "function not found"
        (Just e)  -> case e of
            (Function _ ps _) -> do
                if length ps /= length as then
                    fail "args do not match params"
                else return $ (Val $ Str "no")
            _ -> fail $ nm ++ " is not a function"


statement :: Parser' (Map, Expr)
statement = do
    e <- fnCall <?> fnDef <?> varDef
    st <- getState
    return (st, e)


eval :: Map -> Expr -> Value
eval _ (Function _ _ _) = Void
eval _ (Val v) = v
eval m (Var n) = case M.lookup n m of
    (Nothing) -> Void
    (Just x)  -> eval m x


inlnPrompt :: String -> IO String
inlnPrompt p = do
    _ <- putStr p
    _ <- hFlush stdout
    getLine


run :: IO ()
run = hSetEcho stdin False >> (loop M.empty)


loop :: Map -> IO ()
loop mp = inlnPrompt "~>>" >>= (\inp -> case inp of
    "exit"  -> exitSuccess
    "stop"  -> return ()
    "clear" -> loop M.empty
    _       -> case parse statement inp mp of
        (Failure e)      ->
            (putStrLn $ "!>> " ++ e) >> loop mp
        (Success (m, e)) ->
            (putStrLn $ "=>> " ++ show (eval m e)) >> loop m)
