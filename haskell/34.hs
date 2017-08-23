{-
Problem 34

(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

Example:

* (totient-phi 10)
4

Example in Haskell:

* totient 10
4

-}

gcd' :: Integer -> Integer -> Integer
gcd' m n = if n == 0 then m
           else gcd n (m `mod` n)

coprime :: Integer -> Integer -> Bool
coprime m n = (gcd' m n) == 1

totient :: Integer -> Int
totient x = length $ filter (\b->b) $ map (\y->coprime y x) [1 .. (x - 1)]
