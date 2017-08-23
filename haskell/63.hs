-- Problem 63
-- Construct a complete binary tree

-- A complete binary tree with height H is defined as follows:

-- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
-- In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

-- We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.

-- Write a predicate complete_binary_tree/2.

-- Example:

-- % complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
-- Example in Haskell:

-- Main> completeBinaryTree 4
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
 
-- Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
-- True

import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n =
  if n == 0 then Empty
  else if n == 1 then Branch 'x' Empty Empty
  else 
    let roundUp = iterate (*2) 1  -- [1,2,4,8,16..]
        roundUpSum = sum $ takeWhile (<=n) roundUp
        roundUpInitSum = sum $ init $ takeWhile (<=n) roundUp
        nLastLineMax = roundUpSum - roundUpInitSum
        nLastLine = n - roundUpInitSum
        reachHalf = nLastLine >= nLastLineMax `div` 2
        ground = roundUpInitSum `div` 2
    in
        if reachHalf then
          Branch 'x'
          (completeBinaryTree (ground + nLastLineMax `div` 2))
          (completeBinaryTree (ground + nLastLine - (nLastLineMax `div` 2)))
        else
          Branch 'x'
          (completeBinaryTree (ground + nLastLine))
          (completeBinaryTree ground)

binaryTreeHeight :: Tree a -> Int
binaryTreeHeight tree =
  case tree of
   Empty -> 0
   Branch _ l r -> 1 + (max (binaryTreeHeight l) (binaryTreeHeight r))
  
isCompleteBinaryTree1 :: Tree a -> Int -> Int -> Bool
isCompleteBinaryTree1 tree layout height =
  if layout < height - 1 then
    case tree of
     Empty -> False
     Branch _ Empty _ -> False
     Branch _ _ Empty -> False
     Branch _ l r -> (isCompleteBinaryTree1 l (layout + 1) height) &&
                     (isCompleteBinaryTree1 r (layout + 1) height)
  else True
         
grabFinalLine :: Tree a -> Int -> Int -> [Bool]
grabFinalLine tree@(Branch _ l r) layout height =
  if height == 1 then
    [True]
  else
   if layout < height - 1 then
     grabFinalLine l (layout + 1) height ++
     grabFinalLine r (layout + 1) height
   else if layout == height - 1 then
          case tree of
           Branch _ Empty Empty -> [False, False]
           Branch _ Empty _ -> [False, True]
           Branch _ _ Empty -> [True, False]
           Branch _ _ _ -> [True, True]
           Empty -> [False, False]
        else []

isCompleteBinaryTree2 :: Tree a -> Int -> Bool
isCompleteBinaryTree2 tree height =
  let finalLine = grabFinalLine tree 1 height
  in ((reverse .sort) finalLine == finalLine) && -- Aligned
     (((length . group) finalLine) <= 2) -- Full or Complete

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree tree =
  let height = binaryTreeHeight tree
   in isCompleteBinaryTree1 tree 1 height &&
     isCompleteBinaryTree2 tree height
  
