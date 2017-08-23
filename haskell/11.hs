{-
Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

-}

data Item a = Multiple Int a | Single a
            deriving (Show)

encodeModified :: Eq a => [a] -> [Item a]
encodeModified lst = if length lst == 0 then []
           else if length lst == 1 then [Single (head lst)]
           else let lasts = 1 + headLasts lst
                    clipPart = if lasts == 1 then Single (head lst)
                               else Multiple lasts (head lst)
                    restPart = encodeModified $ drop lasts lst
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


