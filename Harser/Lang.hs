module Harser.Lang where

import Harser.Parser (Parser(..))


data Assoc
    = AssocLeft
    | AssocNone
    | AssocRight


data Operator s u a
    = Infix Assoc (Parser s u (a -> a -> a))
    | Prefix (Parser s u (a -> a))
    | Postfix (Parser s u (a -> a))


type OperatorTable s u a = [[Operator s u a]]


data GenLangParser s u = GenLangParser {
    commentStart :: String,
    commentEnd :: String,
    commentInline :: String,
    idStart :: Parser s u Char,
    idValid :: Parser s u Char,
    resNames :: [String],
    operators :: [String]
}


data GenTokenParser s u = GenTokenParser {
    iden :: Parser s u String,
    oper :: Parser s u String,
    charLit :: Parser s u Char,
    strLit :: Parser s u String,
    integer :: Parser s u Integer,
    float :: Parser s u Double,
    symbol :: String -> Parser s u String,
    stmtSep :: Parser s u String
}