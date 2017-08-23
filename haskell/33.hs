{-
Problem 33

(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

Example:

* (coprime 35 64)
T

Example in Haskell:

* coprime 35 64
True

-}

gcd' :: Integer -> Integer -> Integer
gcd' m n = if n == 0 then m
           else gcd n (m `mod` n)

coprime :: Integer -> Integer -> Bool
coprime m n = (gcd' m n) == 1
