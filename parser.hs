
data Operator = PLUS | MINUS | MULTIPLY | DIVIDE
    deriving Show

data Node = 
    Node {
        operator :: Operator,
        leftChild :: Maybe Node,
        rightChild :: Maybe Node
    } deriving Show

getOperator :: Node -> Operator
getOperator (Node o _ _) = o

getLeftChild :: Node -> Maybe Node
getLeftChild (Node _ l _) = l

getRightChild :: Node -> Maybe Node
getRightChild (Node _ l _) = l

main = do
    f <- readFile "input.scm"
    putStrLn f
