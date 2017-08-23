{-
Problem 24

Lotto: Draw N different random numbers from the set 1..M.

Example:

* (rnd-select 6 49)
(23 1 17 33 21 37)

Example in Haskell:

Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]

-}

import System.Random

diffSelectHelper :: StdGen -> Int -> Int -> [Int] -> [Int]
diffSelectHelper g n hi lst = if n == 0 then lst
                       else let (v, nextg) = randomR (0,hi) g
                            in
                             if v `elem` lst then
                               diffSelectHelper nextg n hi lst
                             else
                               diffSelectHelper nextg (n - 1) hi (lst ++ [v])

diffSelect :: Int -> Int -> [Int]
diffSelect n hi = let g = mkStdGen 0
                  in diffSelectHelper g n hi []

