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
    | ListValue [Value]
    | PLUS
    | MINUS
    | IF
    | CONS
    | CAR
    | CDR
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

-- split the string at the dilimiter and returns a 3-tuple containing
-- elements before the dilimiter, the dilimiter and elements after it
splitAtNextDil :: String -> (String, String, String)
splitAtNextDil = splitAtNext $ dils where
    dils = [' ', '\n', '(', ')']
    splitAtNext dils str = go str "" where
        go [] buffer = (buffer, "", "")
        go (x:xs) buffer
            | x `elem` dils = (buffer, [x], xs)
            | otherwise = go xs (buffer ++ [x])

-- match a single string to the corresponding token
matchToken :: String -> Maybe Token
matchToken t
    | (t `elem` opertors) = Just (NameToken t)
    | (numValue /= "") = Just (NumberToken $ (read numValue :: Double))
    | otherwise = Nothing where
        numValue = (t =~ "^[-+]?[0-9]+\\.?[0-9]*$" :: String)
        opertors = ["(", ")", "+", "-", "if", "cons", "car", "cdr"]

-- convert a expression string into a list of tokens
tokenize :: String -> [Token]
tokenize = map (maybe (error "tokenizer error") id) . filter isJust . _tokenize where
    _tokenize [] = []
    _tokenize s = map matchToken [pre, delim] ++ _tokenize post where
        (pre, delim, post) = splitAtNextDil s

-- convert a list of tokens to an list of expressions with the remaining tokens
parseExprList :: [Token] -> ([Expression], [Token])
parseExprList toks = go [] toks where
    go results ((NameToken ")") : remainToks) = (results, remainToks)
    go results remainToks = 
        go (results ++ [retExpr]) retRemainToks where
            (retExpr, retRemainToks) = parseExpr remainToks

-- convert a list of tokens to an expression with the remaining tokens
parseExpr :: [Token] -> (Expression, [Token])
parseExpr (expr : post) =
    case expr of
        NumberToken n -> (ValueExpr (DoubleValue n), post)
        NameToken "+" -> (ValueExpr PLUS, post)
        NameToken "-" -> (ValueExpr MINUS, post)
        NameToken "if" -> (ValueExpr IF, post)
        NameToken "cons" -> (ValueExpr CONS, post)
        NameToken "car" -> (ValueExpr CAR, post)
        NameToken "cdr" -> (ValueExpr CDR, post)
        NameToken "(" -> (ListExpr exprList, remainToks) where
            (exprList, remainToks) = parseExprList post

-- evaluate a list of expressions by calling eval to each sub-expression
evalList :: [Expression] -> Value
evalList (op : car : cdr) = 
    case (eval op) of
        PLUS -> foldl plus (eval car) (map eval cdr)
        MINUS -> foldl minus (eval car) (map eval cdr)
        CONS -> ListValue [(eval car), (eval $ head cdr)]
        IF -> if (eval car) /= (DoubleValue 0) 
                then eval $ head cdr 
                else eval $ head $ tail cdr

-- eval an expression, returning a type Value
eval :: Expression -> Value
eval expr = 
    case expr of
        (ValueExpr num) -> num
        (ListExpr l) -> evalList l

main = do
    f <- readFile "input.scm"
    (putStrLn . show . tokenize) f
    (putStrLn . show . eval . fst . parseExpr . tokenize) f

