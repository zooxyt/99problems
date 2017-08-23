 -- Problem 70C
-- (*) Count the nodes of a multiway tree.

-- Example in Haskell:

-- Tree> nnodes tree2
-- 2

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ subnodes) = 1 + (sum $ map nnodes subnodes)
