import Data.List

data Operator = PLUS | MINUS | MULTIPLY | DIVIDE | IF deriving Read

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

main = do
    let v1 = ValueNode 1
    let v2 = ValueNode 2
    let v3 = ValueNode 3
    let o1 = OperatorNode PLUS v1 [v2, v3]
    let o2 = OperatorNode MINUS v2 [o1, v3]

    putStrLn $ show o2

    putStrLn $ show $ eval o2

    f <- readFile "input.scm"
    putStrLn f

