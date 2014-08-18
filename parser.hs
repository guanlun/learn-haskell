import Data.List
import Data.Maybe (isJust, isNothing)
import Debug.Trace
import Text.Printf
import Text.Regex.Posix

data Token = 
    Number Double
    | L_BRKT 
    | R_BRKT 
    | PLUS
    | MINUS
    | MULTIPLY
    | DIVIDE
    | INVALID_TOKEN 
    deriving Eq

instance Show Token where
    show L_BRKT = "("
    show R_BRKT = ")"
    show PLUS = "+"
    show MINUS = "-"
    show MULTIPLY = "*"
    show DIVIDE = "/"
    show INVALID_TOKEN = "__INVALID_TOKEN__"
    show (Number num) = show num

data Expression =
    ListExpr [Expression]
    | DoubleExpr Double
    deriving Show

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

-- 
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
matchToken "+" = Just PLUS
matchToken "(" = Just L_BRKT
matchToken ")" = Just R_BRKT
matchToken t
    | (numValue /= "") = Just (Number $ (read numValue :: Double))
    | otherwise = Nothing where
        numValue = (t =~ "^[-+]?[0-9]+\\.?[0-9]*$" :: String)

_tokenize :: String -> [Maybe Token]
_tokenize [] = []
_tokenize s = map matchToken [pre, delim] ++ _tokenize post where
    (pre, delim, post) = splitAtNextDil s

tokenize :: String -> [Token]
tokenize = map (maybe INVALID_TOKEN id) . filter isJust . _tokenize

untilNext :: (Eq a) => a -> [a] -> [a]
untilNext dil l = go l [] where
    go [] buffer = buffer
    go (x:xs) buffer
        | x == dil = buffer
        | otherwise = go xs (buffer ++ [x])

untilNextRBrkt :: [Token] -> [Token]
untilNextRBrkt = untilNext R_BRKT

{-
parseOprt :: [Token] -> Node
parseOprt (x:y:xs) =
    OperatorNode

parseExpr :: [Token] -> Node
parseExpr (x:xs) = 
    case x of
        (Number n) -> ValueNode n
        L_BRKT -> OperatorNode 
        R_BRKT -> 
parseExpr _ = ValueNode 0

parseMany :: [Token] -> [Node]
-}

-- parseBrktPair :: [Token] -> Node

-- (op expr expr expr expr)
-- (+ 1 (+ 2 3))
-- (if (1 < 2) 3 4)
-- (define x (lambda (y) (+ y 1)))
-- if-node 
--  car: (1 < 2)

-- parse :: [Token] -> Node
-- parse = parseBrktPair
--

parseExpr :: [Token] -> (Expression, [Token])
parseExpr (expr : post) =
    case expr of
        Number n -> (DoubleExpr n, post)
        L_BRKT -> (ListExpr exprList, remainToks) where
            (exprList, remainToks) = parseExprList post

parseExprList :: [Token] -> ([Expression], [Token])
parseExprList toks = go [] toks where
    go results (R_BRKT : remainToks) = (results, remainToks)
    go results remainToks = 
        go (results ++ [retExpr]) retRemainToks where
            (retExpr, retRemainToks) = parseExpr remainToks

main = do
    f <- readFile "input.scm"
    putStrLn $ show $ fst $ parseExpr $ tokenize f

