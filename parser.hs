import Data.List
import Data.Maybe (isJust, isNothing)
import Debug.Trace
import Text.Printf
import Text.Regex.Posix

data Token = 
    Number Double
    | Name String
    | LBrkt_t
    | RBrkt_t
    | Plus_t
    | Minus_t
    | Multiply_t
    | Divide_t
    | InvalidToken_t
    deriving Eq

instance Show Token where
    show LBrkt_t = "("
    show RBrkt_t = ")"
    show Plus_t = "+"
    show Minus_t = "-"
    show Multiply_t = "*"
    show Divide_t = "/"
    show InvalidToken_t = "__INVALID_TOKEN__"
    show (Number num) = show num

data Operator = 
    Plus_o
    | Minus_o
    | Multiply_o
    | Divide_o
    deriving Show

data Expression =
    ListExpr [Expression]
    | DoubleExpr Double
    | OperatorExpr Operator
    deriving Show

{-
getOprt :: (Floating a) => Token -> (a -> a -> a)
getOprt PLUS = (+)
getOprt MINUS = (-)
getOprt MULTIPLY = (*)
getOprt DIVIDE = (/)

data Node = 
    ValueNode {
        value :: Double
    }
    | OperatorNode {
        operator :: Token,
        car :: Node, 
        cdr :: [Node]
    }

instance Show Node where
    show (ValueNode v) = show v
    show (OperatorNode o car cdr) = "( " ++ show o ++ " " ++ 
        show car ++ " " ++ intercalate " " (map show cdr) ++ " )"

eval :: Node -> Double
eval (ValueNode i) = i
eval (OperatorNode oprt car cdr) = foldl op (eval car) (map eval cdr)
    where op = getOprt oprt
-}

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
matchToken "+" = Just Plus_t
matchToken "-" = Just Minus_t
matchToken "*" = Just Multiply_t
matchToken "/" = Just Divide_t
matchToken "(" = Just LBrkt_t
matchToken ")" = Just RBrkt_t
matchToken t
    | (numValue /= "") = Just (Number $ (read numValue :: Double))
    | otherwise = Nothing where
        numValue = (t =~ "^[-+]?[0-9]+\\.?[0-9]*$" :: String)

_tokenize :: String -> [Maybe Token]
_tokenize [] = []
_tokenize s = map matchToken [pre, delim] ++ _tokenize post where
    (pre, delim, post) = splitAtNextDil s

tokenize :: String -> [Token]
tokenize = map (maybe InvalidToken_t id) . filter isJust . _tokenize

untilNext :: (Eq a) => a -> [a] -> [a]
untilNext dil l = go l [] where
    go [] buffer = buffer
    go (x:xs) buffer
        | x == dil = buffer
        | otherwise = go xs (buffer ++ [x])

untilNextRBrkt_t :: [Token] -> [Token]
untilNextRBrkt_t = untilNext RBrkt_t

parseExpr :: [Token] -> (Expression, [Token])
parseExpr (expr : post) =
    case expr of
        Plus_t -> (OperatorExpr Plus_o, post)
        Minus_t -> (OperatorExpr Minus_o, post)
        Number n -> (DoubleExpr n, post)
        LBrkt_t -> (ListExpr exprList, remainToks) where
            (exprList, remainToks) = parseExprList post

parseExprList :: [Token] -> ([Expression], [Token])
parseExprList toks = go [] toks where
    go results (RBrkt_t : remainToks) = (results, remainToks)
    go results remainToks = 
        go (results ++ [retExpr]) retRemainToks where
            (retExpr, retRemainToks) = parseExpr remainToks

evalList :: [Expression] -> Double
evalList (op : car : cdr) = 
    case op of
        OperatorExpr Plus_o -> foldl (+) (eval car) (map eval cdr)
        OperatorExpr Minus_o -> foldl (-) (eval car) (map eval cdr)

eval :: Expression -> Double
eval expr = 
    case expr of
        (DoubleExpr num) -> num
        (ListExpr l) -> evalList l
        otherwise -> 0

main = do
    f <- readFile "input.scm"
    (putStrLn . show . eval . fst . parseExpr . tokenize) f

