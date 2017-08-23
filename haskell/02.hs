{-
Problem 2

(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'

-}

myButLast :: [a] -> a
myButLast x = if length x < 2 then error "expect list with 2 or more elements"
              else x !! (length x - 2)
