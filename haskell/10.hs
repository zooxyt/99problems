{-
Problem 10

(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

Example in Haskell:

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

-}

encode :: Eq a => [a] -> [(Int, a)]
encode lst = if length lst == 0 then []
           else if length lst == 1 then [(1, head lst)]
           else let lasts = 1 + headLasts lst
                    clipPart = (lasts, head lst)
                    restPart = encode $ drop lasts lst
                in
                 if length restPart == 0 then [clipPart]
                 else clipPart : restPart
  where
    headLasts :: Eq a => [a] -> Int
    headLasts [] = 0
    headLasts (x:xs) = let 
      foo :: Eq a => a -> [a] -> Int
      foo _ [] = 0
      foo c s = if head s == c then 1 + (foo c (tail s)) else 0
      in
       foo x xs


