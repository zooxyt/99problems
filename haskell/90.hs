{-
Problem 90
(**) Eight queens problem

This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Example in Haskell:

> length (queens 8)
92
> head (queens 8)
[1,5,8,6,3,7,2,4]

-}

import Data.List (permutations)

testOne :: Int -> [Int] -> Bool
testOne z arr = length [(x,y) |
                        x <- [0..(length arr) - 1],
                        y <- [0..(length arr) - 1],
                        x + y == z,
                        arr !! x == y] <= 1

testTwo :: Int -> [Int] -> Bool
testTwo z arr = length [(x,y) |
                        x <- [0..(length arr) - 1],
                        y <- [0..(length arr) - 1],
                        x + y == z,
                        arr !! ((length arr) - x - 1) == y] <= 1

testThree :: Int -> [Int] -> Bool
testThree z arr = length [(x,y) |
                        x <- [0..(length arr) - 1],
                        y <- [0..(length arr) - 1],
                        x + y == z,
                        arr !! x == (length arr) - y - 1] <= 1

testFour :: Int -> [Int] -> Bool
testFour z arr = length [(x,y) |
                        x <- [0..(length arr) - 1],
                        y <- [0..(length arr) - 1],
                        x + y == z,
                        arr !! ((length arr) - x - 1) == (length arr) - y - 1] <= 1

test :: [Int] -> Bool
test arr = length arr == (length $ filter (\x->x) $ map
                          (\z->(testOne z arr) &&
                               (testTwo z arr) &&
                               (testThree z arr) &&
                               (testFour z arr)) [0..(length arr - 1)])

queens :: Int -> [[Int]]
queens n = map (\y-> map (\x->x+1) y) $ filter (\x->test x) $ permutations [0..n-1]

