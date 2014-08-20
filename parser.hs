import Data.List
import Data.Maybe (isJust, isNothing)
import Text.Regex.Posix

data Token = 
    NumberToken Double
    | NameToken String
    deriving Eq

instance Show Token where
    show (NumberToken num) = show num
    show (NameToken str) = show str

data Value = 
    DoubleValue Double
    | PLUS
    | MINUS
    | IF
    deriving Show

plus :: Value -> Value -> Value
plus (DoubleValue a) (DoubleValue b) = DoubleValue (a + b)

minus :: Value -> Value -> Value
minus (DoubleValue a) (DoubleValue b) = DoubleValue (a - b)

instance Eq Value where
    DoubleValue a == DoubleValue b = a == b

data Expression =
    ListExpr [Expression]
    | ValueExpr Value
    deriving Show

splitAtNext :: [Char] -> String -> (String, String, String)
splitAtNext dils str = go str "" where
    go [] buffer = (buffer, "", "")
    go (x:xs) buffer
        | x `elem` dils = (buffer, [x], xs)
        | otherwise = go xs (buffer ++ [x])

nullDils = [' ', '\n']
tokenDils = ['(', ')']

splitAtNextDil :: String -> (String, String, String)
splitAtNextDil = splitAtNext $ nullDils ++ tokenDils

matchToken :: String -> Maybe Token
matchToken t
    | (t `elem` opertors) = Just (NameToken t)
    | (numValue /= "") = Just (NumberToken $ (read numValue :: Double))
    | otherwise = Nothing where
        numValue = (t =~ "^[-+]?[0-9]+\\.?[0-9]*$" :: String)
        opertors = ["(", ")", "+", "-", "if"]

_tokenize :: String -> [Maybe Token]
_tokenize [] = []
_tokenize s = map matchToken [pre, delim] ++ _tokenize post where
    (pre, delim, post) = splitAtNextDil s

tokenize :: String -> [Token]
tokenize = map (maybe (error "tokenizer error") id) . filter isJust . _tokenize

untilNext :: (Eq a) => a -> [a] -> [a]
untilNext dil l = go l [] where
    go [] buffer = buffer
    go (x:xs) buffer
        | x == dil = buffer
        | otherwise = go xs (buffer ++ [x])

untilNextRBrkt_t :: [Token] -> [Token]
untilNextRBrkt_t = untilNext (NameToken ")")

parseExpr :: [Token] -> (Expression, [Token])
parseExpr (expr : post) =
    case expr of
        (NumberToken n) -> (ValueExpr (DoubleValue n), post)
        (NameToken "+") -> (ValueExpr PLUS, post)
        (NameToken "-") -> (ValueExpr MINUS, post)
        (NameToken "if") -> (ValueExpr IF, post)
        (NameToken "(") -> (ListExpr exprList, remainToks) where
            (exprList, remainToks) = parseExprList post

parseExprList :: [Token] -> ([Expression], [Token])
parseExprList toks = go [] toks where
    go results ((NameToken ")") : remainToks) = (results, remainToks)
    go results remainToks = 
        go (results ++ [retExpr]) retRemainToks where
            (retExpr, retRemainToks) = parseExpr remainToks

evalList :: [Expression] -> Value
evalList (op : car : cdr) = 
    case (eval op) of
        PLUS -> foldl plus (eval car) (map eval cdr)
        MINUS -> foldl minus (eval car) (map eval cdr)
        IF -> 
            if (eval car) /= (DoubleValue 0) 
                then eval $ head cdr 
                else eval $ head $ tail cdr

eval :: Expression -> Value
eval expr = 
    case expr of
        (ValueExpr num) -> num
        (ListExpr l) -> evalList l

main = do
    f <- readFile "input.scm"
    (putStrLn . show . eval . fst . parseExpr . tokenize) f

