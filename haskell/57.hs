-- Problem 57
-- (**) Binary search trees (dictionaries)

-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

-- Example:

-- * construct([3,2,5,7,1],T).
-- T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
-- Then use this predicate to test the solution of the problem P56.

-- Example:

-- * test-symmetric([5,3,18,1,4,12,21]).
-- Yes
-- * test-symmetric([3,2,5,7,4]).
-- No
-- Example in Haskell:

-- *Main> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-- *Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- *Main> symmetric . construct $ [3, 2, 5, 7, 1]
 -- True

import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

data BranchDirection = LeftBranch | RightBranch deriving (Eq)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l0 l1) (Branch _ r0 r1) = (mirror l0 r1) && (mirror l1 r0)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric tree =
  case tree of
   Empty -> True
   (Branch _ x y) -> mirror x y


bsTree :: Ord a => [a] -> BranchDirection -> Tree a
bsTree lst d =
  let n = length lst
  in if n == 0 then Empty
     else if n `mod` 2 /= 0 then
            let lbranch = bsTree (take (n `div` 2) lst) LeftBranch
                rbranch = bsTree (drop (n `div` 2 + 1) lst) RightBranch
            in Branch (lst !! (n `div` 2)) lbranch rbranch
          else
            if d == LeftBranch then
             let lbranch = bsTree (take (n `div` 2) lst) LeftBranch
                 rbranch = bsTree (drop (n `div` 2 + 1) lst) RightBranch
                 merged = Branch (lst !! (n `div` 2)) lbranch rbranch
             in merged
            else
             let lbranch = bsTree (take (n `div` 2 - 1) lst) LeftBranch
                 rbranch = bsTree (drop (n `div` 2) lst) RightBranch
                 merged = Branch (lst !! (n `div` 2 - 1)) lbranch rbranch
             in merged
            
construct :: Ord a => [a] -> Tree a
construct lst = bsTree (sort lst) LeftBranch
