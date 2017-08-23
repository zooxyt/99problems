{-
Problem 35

(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

Example:

* (prime-factors 315)
(3 3 5 7)

Example in Haskell:

> primeFactors 315
[3, 3, 5, 7]

Solutions

-}

primeFactors :: Int -> [Int]
primeFactors 1 = [1]
primeFactors x = let z1 = head $ filter (\y->x `mod` y == 0) [2..]
                     z2 = x `div` z1
                 in if z2 == 1 then [z1] else z1 : primeFactors z2
