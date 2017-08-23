-- Problem 61
-- Count the leaves of a binary tree

-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

-- Example:

-- % count_leaves(T,N) :- the binary tree T has N leaves
-- Example in Haskell:

-- > countLeaves tree4
-- 2

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

countLeaves :: Tree a -> Int
countLeaves tree =
  case tree of
   Empty -> 0
   Branch _ Empty Empty -> 1
   Branch _ x y -> countLeaves x + countLeaves y

