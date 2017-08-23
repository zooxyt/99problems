-- Problem 61A
-- Collect the leaves of a binary tree in a list

-- A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

-- Example:

-- % leaves(T,S) :- S is the list of all leaves of the binary tree T
-- Example in Haskell:

-- > leaves tree4
-- [4,2]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaves :: Tree a -> [a]
leaves tree =
  case tree of
   Empty -> []
   Branch c Empty Empty -> [c]
   Branch _ x y -> leaves x ++ leaves y
  
