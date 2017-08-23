{-
Problem 1

(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'

-}

myLast :: [a] -> a
myLast [] = error "expect list with at least 1 element"
myLast x = if length x == 1 then x !! 0 else myLast $ tail x
