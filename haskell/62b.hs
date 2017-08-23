-- Problem 62B
-- Collect the nodes at a given level in a list

-- A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

-- Example:

-- % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
-- Example in Haskell:

-- Prelude>atLevel tree4 2
-- Prelude>[2,2]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

atLevel :: Tree a -> Int -> [a]
atLevel tree level = atLevel' tree level 1
  where
    atLevel' tree level cur_level =
      case tree of
       Empty -> []
       Branch ch x y -> if cur_level == 2 then [ch]
                        else if cur_level < 2 then atLevel' x level (cur_level + 1) ++
                                               atLevel' y level (cur_level + 1)
                             else []
                   
  
