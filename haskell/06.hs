{-
Problem 6

(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:

*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True

-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = if length x <= 1 then True
                 else let theFirst = head x
                          theFinal = last x
                      in
                       if theFirst == theFinal then
                         isPalindrome $ tail $ init x
                       else
                         False
