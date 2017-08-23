{-
Problem 23

Extract a given number of randomly selected elements from a list.

Example:

* (rnd-select '(a b c d e f g h) 3)
(E D A)

Example in Haskell:

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda

-}

import System.Random


fooHelper :: StdGen -> Int -> Int -> [Int] -> [Int]
fooHelper g n hi lst = if n == 0 then lst
                       else let (v, nextg) = randomR (0,hi) g
                            in
                             if v `elem` lst then
                               fooHelper nextg n hi lst
                             else
                               fooHelper nextg (n - 1) hi (lst ++ [v])

randSelectors :: Int -> Int -> [Int]
randSelectors n hi = let g = mkStdGen 0
                     in fooHelper g n hi []

rndSelect :: [a] -> Int -> [a]
rndSelect lst n = map (\idx->lst !! idx) $ randSelectors n (length lst - 1)

