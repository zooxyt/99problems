-- Problem 47

-- (*) Truth tables for logical expressions (2).

-- Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

-- Example:

-- * (table A B (A and (A or not B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail

-- Example in Haskell:

-- > table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False

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

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
  let input_set = [(True, True), (True, False), (False, True), (False, False)]
      g x = (show (fst x)) ++ " " ++ (show (snd x)) ++ " " ++ (show $ f (fst x) (snd x))
      output_set = map g input_set
  mapM_ putStrLn output_set

infixl 4 `or'`
infixl 6 `and'`
