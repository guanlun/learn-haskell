
data Operator = PLUS | MINUS | MULTIPLY | DIVIDE

instance Show Operator where
    show PLUS = "+"
    show MINUS = "-"
    show MULTIPLY = "*"
    show DIVIDE = "/"

data Node = 
    IntNode {
        value :: Int
    }
    | OperatorNode {
        operator :: Operator,
        leftChild :: Node, 
        rightChild :: Node
    }

instance Show Node where
    show (IntNode v) = show v
    show (OperatorNode o l r) = "( " ++ show o ++ " " ++ 
        show l ++ " " ++ show r ++ " )"

eval :: Node -> Int
eval (IntNode i) = i
eval (OperatorNode PLUS l r) = (eval l) + (eval r)
eval (OperatorNode MINUS l r) = (eval l) - (eval r)
eval (OperatorNode MULTIPLY l r) = (eval l) * (eval r)
eval (OperatorNode DIVIDE l r) = (eval l) / (eval r)

main = do
    f <- readFile "input.scm"
    putStrLn f
