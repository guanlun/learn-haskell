import Data.List
import Data.Maybe (isJust, isNothing)

data Operator = PLUS | MINUS | MULTIPLY | DIVIDE | IF deriving Read

data Token = L_BRKT | R_BRKT | PLUS_ | MINUS_ | INVALID_TOKEN deriving Show

instance Show Operator where
    show PLUS = "+"
    show MINUS = "-"
    show MULTIPLY = "*"
    show DIVIDE = "/"
    show IF = "if"

getOprt :: (Floating a) => Operator -> (a -> a -> a)
getOprt PLUS = (+)
getOprt MINUS = (-)
getOprt MULTIPLY = (*)
getOprt DIVIDE = (/)

data Node = 
    ValueNode {
        value :: Double
    }
    | OperatorNode {
        operator :: Operator,
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

split :: String -> String -> [String]
split dils str = reverse $ go str "" [] where
    go [] buffer results = results ++ [buffer]
    go (x:xs) buffer results
        | x `elem` dils = go xs [] results ++ [buffer]
        | otherwise = go xs (buffer ++ [x]) results

splitBySpace :: String -> [String]
splitBySpace = split " "

splitAtNext :: String -> String -> [String]
splitAtNext dils str = go str "" where
    go [] buffer = [buffer, "", ""]
    go (x:xs) buffer
        | x `elem` dils = [buffer, [x], xs]
        | otherwise = go xs (buffer ++ [x])

nullDils = [' ', '\n']
tokenDils = ['(', ')']

splitAtNextDil :: String -> [String]
splitAtNextDil = splitAtNext $ nullDils ++ tokenDils

matchToken :: String -> Maybe Token
matchToken "+" = Just PLUS_
matchToken "(" = Just L_BRKT
matchToken ")" = Just R_BRKT
matchToken _ = Nothing

{-
_tokenize :: String -> [String]
_tokenize [] = []
_tokenize s = nextSeg : dil : tokenize remain where
    nextSeg = splitted !! 0 where
        splitted = splitAtNextDil s
    dil = splitted !! 1 where
        splitted = splitAtNextDil s
    remain = splitted !! 2 where
        splitted = splitAtNextDil s
-}

_tokenize :: String -> [Maybe Token]
_tokenize [] = []
_tokenize s = map matchToken (init splitted) ++ (_tokenize $ last splitted) where
    splitted = splitAtNextDil s

tokenize :: String -> [Token]
tokenize = map (maybe INVALID_TOKEN id) . filter isJust . _tokenize

main = do
    let v1 = ValueNode 1
    let v2 = ValueNode 2
    let v3 = ValueNode 3
    let o1 = OperatorNode PLUS v1 [v2, v3]
    let o2 = OperatorNode MINUS v2 [o1, v3]

    -- putStrLn $ show o2
    -- putStrLn $ show $ eval o2

    f <- readFile "input.scm"
    putStrLn $ show $ tokenize f

