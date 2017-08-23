{-
Problem 9

(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))

Example in Haskell:

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]

-}

pack :: Eq a => [a] -> [[a]]
pack lst = if length lst == 0 then []
           else if length lst == 1 then [lst]
           else let lasts = 1 + headLasts lst
                    clipPart = take lasts lst
                    restPart = pack $ drop lasts lst
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

