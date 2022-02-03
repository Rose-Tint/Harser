module Examples.Json (run) where

import Prelude hiding (map, null)

import System.IO (hFlush, hSetEcho, stdout, stdin)
import System.Exit (exitSuccess)

import Harser.Char hiding (string)
import qualified Harser.Char as HC (string)
import Harser.Combinators
import Harser.Parser
import Harser.Utilities


data Object
    = Object [(String, Object)]
    | List [Object]
    | String String
    | Number Float
    | Boolean Bool
    | Null
    deriving (Show, Eq)


type Parser' a = Parser String () a

type Parser'' = Parser' Object


object :: Parser''
object = choose [
        string,
        number,
        list,
        map,
        boolean,
        null
    ]


string :: Parser''
string = String <$> string'


string' :: Parser' String
string' = wrap (char '"') (zeroOrMore anyChar)


number :: Parser''
number = Number <$> (fractional <?> (fromIntegral <$> integral))


list :: Parser''
list = List <$> brackets' (comma `splits` object)


map :: Parser''
map = Object <$> braces' (comma `splits` pair)
    where
        pair = do
            key <- string'
            _ <- wrap skipws (char ':')
            value <- object
            return (key, value)


boolean :: Parser''
boolean = Boolean <$> choose [
        HC.string "true"  >> pure True,
        HC.string "false" >> pure False
    ]


null :: Parser''
null = HC.string "null" *> return Null


comma :: Parser' ()
comma = wrap skipws (char ',') >> pure ()


tabs :: Int -> String
tabs n = replicate (n * 4) ' '


show' :: Int -> Object -> String
-- show' n Null = "null"
-- show' n (Boolean True) = "true"
-- show' n (Boolean False) = "false"
-- show' n (Number n) =
--     if n == fromInteger (round n) then
--         show (fromInteger n)
--     else
--         show n
-- show' n (String s) = show s
-- show' n (List l) =
--     printf "[\n%s%s\n%s]" ntabs ls' ntabs
--         where
--             ntabs = tabs n
--             ls' = intercalate ",\n" ls
--             ls = fmap (show (n + 1)) l
-- ...
show' n o = tabs n ++ show o


run :: IO ()
run = do
    inp <- readFile "Examples/example.json"
    print $ parse object inp ()
