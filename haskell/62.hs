 -- Problem 62
-- Collect the internal nodes of a binary tree in a list

-- An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

-- Example:

-- % internals(T,S) :- S is the list of internal nodes of the binary tree T.
-- Example in Haskell:

-- Prelude>internals tree4
-- Prelude>[1,2]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

internals :: Tree a -> [a]
internals tree =
  case tree of
   Empty -> []
   Branch _ Empty Empty -> []
   Branch ch x y -> [ch] ++ internals x ++ internals y
