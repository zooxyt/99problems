-- Problem 48

-- (**) Truth tables for logical expressions (3).

-- Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

-- Example:

-- * (table (A,B,C) (A and (B or C) equ A and B or A and C))
-- true true true true
-- true true fail true
-- true fail true true
-- true fail fail true
-- fail true true true
-- fail true fail true
-- fail fail true true
-- fail fail fail true

-- Example in Haskell:

-- > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
 
-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False

import Data.List
import Control.Monad

not' :: Bool -> Bool
not' False = True
not' True = False

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' x y = not' (and' x y)

nor' :: Bool -> Bool -> Bool
nor' x y = not' (or' x y)

equ' :: Bool -> Bool -> Bool
equ' x y = x == y

xor' :: Bool -> Bool -> Bool
xor' x y = x /= y

impl' :: Bool -> Bool -> Bool
impl' x y = or' (not x) y


branches :: [[a]] -> [[a]]
branches tbl
  | length tbl == 0 = []
  | length tbl == 1 = map (\x->[x]) $ head tbl
  | otherwise = let rest = branches $ tail tbl
                in [x : y | x <- head tbl, y <- rest]
                   
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = do
  let input_set = branches $ replicate n [True, False]
      g x = concat $ intersperse " " (map show (x ++ [f x]))
      output_set = map g input_set
  mapM_ putStrLn output_set

infixl 4 `or'`
infixl 6 `and'`

