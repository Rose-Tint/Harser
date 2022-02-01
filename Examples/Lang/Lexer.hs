module Examples.Lang.Lexer where

import Prelude hiding (fail)

import Harser.Char
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


data Keyword = Pure | Impure
    deriving (Show, Eq)


data Token
    = IntTok Int
    | FloatTok Double
    | StringTok String
    | KwrdTok Keyword
    | IdenTok String
    | LParenTok
    | RParenTok
    | AssignmentTok
    | RtnTypeDeclTok
    | CommaTok
    | ColonTok
    deriving (Show, Eq)


type Lexer a = Parser String () a


lexer :: Lexer [Token]
lexer = zeroOrMore (wrap skipws token)


token :: Lexer Token
token = choose [
        (IntTok <$> integral),
        (FloatTok <$> fractional),
        (StringTok <$> wrap (char '"') (zeroOrMore anyChar)),
        (KwrdTok <$> keyword),
        (IdenTok <$> ident),
        (char '(' >> pure LParenTok),
        (char ')' >> pure RParenTok),
        (string ":=" >> pure AssignmentTok),
        (string "=>" >> pure RtnTypeDeclTok),
        (char ',' >> pure CommaTok),
        (char ':' >> pure ColonTok)
    ]


keyword :: Lexer Keyword
keyword = select string ["pure", "impure"] >>= (\kw ->
    case kw of
        "pure"   -> return Pure
        "impure" -> return Impure
        _        -> fail "keyword")


ident :: Lexer String
ident = ((:) <$> letter <*> zeroOrMore alnum) !> "ident"
