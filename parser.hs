
data Operator = PLUS | MINUS | MULTIPLY | DIVIDE

instance Show Operator where
    show PLUS = "+"
    show MINUS = "+"
    show MULTIPLY = "+"
    show DIVIDE = "+"

data Node = 
    OperatorNode {
        operator :: Operator,
        leftChild :: Node, 
        rightChild :: Node
    }
    | IntNode {
        value :: Int
    }

instance Show Node where
    show (OperatorNode o l r) = "( " ++ show o ++ " " ++ 
        show l ++ " " ++ show r ++ " )"

    show (IntNode v) = show v

main = do
    f <- readFile "input.scm"
    putStrLn f
