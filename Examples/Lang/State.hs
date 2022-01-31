module Examples.Lang.State where

import Prelude hiding (lookup, fail)

import Data.Map (
    lookup,
    insert,
    singleton
    )

import Harser.Parser hiding (State)

import Examples.Lang.Data


alloc :: Var -> Parser' ()
alloc v = amendState $ \(State stk ts) -> case stk of
        []       -> State [singleton (varName v) v] ts
        (st:sts) -> State ((insert (varName v) v st):sts) ts


allocFunc :: String -> [Var] -> Type
          -> Expr -> Bool -> Parser' ()
allocFunc nm ps rtn bd ip = alloc $ Var nm FuncType (
    FuncVal $ Function nm ps rtn bd ip)


free :: Parser' ()
free = amendState $ \(State stk ts) -> case stk of
    []      -> State [] ts
    (_:sts) -> State sts ts


findVar :: String -> Parser' Var
findVar nm = (stack <$> getState) >>= helper
    where
        helper [] = fail " | findVar"
        helper (m:ms) = case lookup nm m of
            Nothing -> helper ms
            Just v  -> return v


findRec :: String -> Parser' Record
findRec nm = (types <$> getState) >>= helper
    where
        helper [] = fail " | findRec"
        helper (m:ms) = case lookup nm m of
            Nothing -> helper ms
            Just r  -> return r


findType :: String -> Parser' Type
findType "Int" = return Integer
findType "Flt" = return Float
findType "Str" = return String
findType nm = RecType <$> findRec nm


findFunc :: String -> Parser' Function
findFunc nm = do
    var <- findVar nm
    case var of
        Var _ FuncType (FuncVal fn) -> return fn
        _ -> fail " | findFunc"


valType :: Value -> Parser' Type
valType (IntVal _)  = return Integer
valType (FltVal _)  = return Float
valType (StrVal _)  = return String
valType (VarVal vn) = varType <$> findVar vn
valType (FuncVal _) = return FuncType
