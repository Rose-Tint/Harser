module Examples.Lang.Data where

import Data.Map (Map)

import Harser.Parser (Parser)


data Var
    = Var {
        varName  :: String,
        varType  :: Type,
        varValue :: Value
    }
    | Par {
        varName  :: String,
        varType  :: Type
    }
    deriving (Show)


data Type
    = String
    | Integer
    | Float
    | FuncType
    -- | RecType Record
    deriving (Show, Eq)


-- data Record
--     = Record {
--         recName  :: String,
--         recAttrs :: [Var]
--     }
--     deriving (Show)


data Function
    = Function {
        fnName   :: String,
        fnParams :: [Var],
        fnReturn :: Type,
        fnBody   :: Expr,
        isPure   :: Bool
    }
    deriving (Show)


data Value
    = IntVal Int
    | FltVal Double
    | StrVal String
    | VarVal String
    | FuncVal Function
    deriving (Show, Eq)


data Expr
    = FuncCall {
        fnCallName :: String,
        fnArgs     :: [Value]
    }
    | FuncDef {
        fnDefName  :: String
    }
    | ValueExpr Value
    deriving (Show)


data State
    = State {
        stack :: [Map String Var]
        -- types :: Map String Record
    }
    deriving (Show)


type Parser' a = Parser String State a


-- instance Eq Record where
--     a == b = recName a == recName b


instance Eq Function where
    a == b = fnName a == fnName b
