module Examples.Lang where

import Prelude hiding (lookup)

import Data.Bits (shift)
import Data.Char (ord, isUpper)
import qualified Data.Set as S

import Harser.Char
import Harser.Combinators
import Harser.Parser hiding (State)
import Harser.Utilities


data Variable = Variable {
    varName  :: String,
    varType  :: Type,
    varValue :: Value
}


data Type
    = String
    | Integer
    | Float
    | FuncType


data Record = Record {
    recName  :: String,
    recAttrs :: [Variable]
}


data Function = Function {
    fnName   :: String,
    fnParams :: [Variable],
    fnBody   :: [AST]
}


data Value
    = IntVal Int
    | FltVal Double
    | StrVal String
    | FunVal Function


data AST
    = Function' Function


type State = [M.Map Int Variable]


type Parser' a = Parser String State a


alloc :: Variable -> Parser' ()
alloc v = amendState add where
    add [] = [M.singleton (hash v, v)]
    add (st:sts) = M.insert (hash v) v st


findFunc :: String -> [Variable] -> Parser' Function
findFunc nm [] = fail (printf
    "function '%s' must have one or more types in signature" nm)
findFunc nm ps = do
    st <- getState
    case M.lookup (combine nm ps) st of
        (Nothing) ->
            fail "error: function signature not found for: " ++ nm
        (Just fn) -> return fn


typeIden :: Parser' String
typeIden = (:) <$> satisfy isUpper <*> zeroOrMore letter


-- | ex: fun foo :: T1 -> T2 -> T3
funcDecl :: Parser' Function
funcDecl = do
    _ <- lexeme $ string "fun"
    nm <- iden
    _ <- wrap skipws (string "::")
    ps <- (wrap skipws (string "->")) `splits` typeIden
    return $ Function nm ps []


funcDef :: Parser' Function
funcDef = do
    _ <- lexeme $ string "def"
    nm <- iden
    





class Hash a where
    hash :: a -> Int

combine :: (Hash a, Hash b) => a -> b -> Int
combine le ri
    = rh + hash () + shift lh 6 + shift rh (-2)
        where
            rh = hash ri
            lh = hash lh

instance Hash () where
    hash _ = 5831

instance Hash Int where
    hash n = n

instance Hash Char where
    hash = hash . ord

instance (Foldable t, Hash a) => Hash (t a) where
    hash l = foldr combine (hash ()) l -- where
        -- hash' ph [] = ph
        -- hash' ph (x:xs) =
        --     hash' (((shift ph 5) + ph) + hash x) xs

instance Hash Variable where
    hash = hash . varName

instance Hash Record where
    hash = hash . recName

instance Hash Function where
    hash (Function nm ps _) = combine nm ps

instance Hash a => Eq a where
    a == b = hash a == hash b

instance Hash a => Ord a where
    a <= b = hash a <= hash b
