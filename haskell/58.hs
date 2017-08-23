-- Problem 58
-- (**) Generate-and-test paradigm

-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

-- Example:

-- * sym-cbal-trees(5,Ts).
-- Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
-- Example in Haskell:

-- *Main> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree n
  | n == 0         = [Empty]
  | n == 1         = [Branch 'x' Empty Empty]
  | n `mod` 2 /= 0 = let branch = cbalTree ((n - 1) `div` 2)
                     in [Branch 'x' x y | x <- branch, y <- branch]
  | otherwise      = let branch0 = cbalTree (n `div` 2 - 1)
                         branch1 = cbalTree (n `div` 2)
                     in [Branch 'x' x y | x <- branch0, y <- branch1] ++
                        [Branch 'x' x y | x <- branch1, y <- branch0]

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l0 l1) (Branch _ r0 r1) = (mirror l0 r1) && (mirror l1 r0)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric tree =
  case tree of
   Empty -> True
   (Branch _ x y) -> mirror x y

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric (cbalTree n)
