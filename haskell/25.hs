{-
Problem 25

Generate a random permutation of the elements of a list.

Example:

* (rnd-permu '(a b c d e f))
(B A D C E F)

Example in Haskell:

Prelude System.Random>rnd_permu "abcdef"
Prelude System.Random>"badcef"

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

rndPermu :: [Int] -> [Int]
rndPermu lst = let g = mkStdGen 0
                   selectors = diffSelectHelper g (length lst) ((length lst) - 1) []
               in
                dragToFront selectors lst
  where
    dragToFront :: [Int] -> [Int] -> [Int]
    dragToFront [] lst = lst
    dragToFront selectors lst = let target = head selectors
                                    nextSelectors = tail selectors
                                    nextLst = [lst !! target] ++ (take target lst) ++ (drop (target + 1) lst)
                                in
                                 dragToFront nextSelectors nextLst
                    
