module Examples.CalcWithVars where

import qualified Data.Map as M
import System.IO

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


type Map = M.Map Char Double

type Parser' a = Parser String Map a


data Expr
    = Var Char
    | Num Double
    | Add Expr Expr
    | Mul Expr Expr


expr :: Parser' (Map, Expr)
expr = do
    ex <- oper <?> letExpr
    st <- getState
    return (st, ex)


num :: Parser' Expr
num = Num <$> fractional <?> (fromInteger <$> integral)


var :: Parser' Expr
var = fmap Var letter


term :: Parser' Expr
term = num <?> var


assign :: Parser' ()
assign = do
    c <- letter
    _ <- zeroOrMore space
    _ <- char '='
    _ <- zeroOrMore space
    n <- num
    modifyState (M.insert c (eval M.empty n))


letExpr :: Parser' Expr
letExpr = do
    _ <- string "let"
    _ <- spaces
    _ <- sepBy (char ';') assign
    _ <- spaces
    _ <- string "in"
    oper


oper :: Parser' Expr
oper = add <?> mul


add :: Parser' Expr
add = do
    le <- term
    _ <- skipws *> char '+' *> skipws
    ri <- term
    return $ Add le ri


mul :: Parser' Expr
mul = do
    le <- term
    _ <- skipws *> char '*' *> skipws
    ri <- term
    return $ Mul le ri


eval :: Map -> Expr -> Double
eval m e = case e of
    (Var c)     -> case M.lookup c m of
        (Nothing) -> error "unknown variable"
        (Just n)  -> n
    (Num n)     -> n
    (Add le ri) -> (eval m le) + (eval m ri)
    (Mul le ri) -> (eval m le) * (eval m ri)


main :: IO ()
main = hSetEcho stdin False >> loop where
    loop = do
        inp <- putStr "~>>" >> hFlush stdout >> getLine
        case parse expr inp M.empty of
            (Failure e)      -> putStrLn $ "!>> " ++ e
            (Success (m, e)) -> case e of
                (Var c) -> case M.lookup c m of
                    (Nothing) -> putStrLn "!>> unknown variable"
                    (Just n)  -> putStrLn $ "=>> " ++ show n
                _       -> putStrLn $ "=>> " ++ show (eval m e)
        loop
