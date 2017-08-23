{-
Problem 31

(**) Determine whether a given integer number is prime.

Example:

* (is-prime 7)
T

Example in Haskell:

P31> isPrime 7
True

Solutions 

-}

isPrime :: Integer -> Bool
isPrime x = foldl (&&) True $ map (\y->(x `mod` y) /= 0) [2 .. floor $ sqrt $ fromInteger x]
