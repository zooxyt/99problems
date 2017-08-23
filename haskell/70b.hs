-- Problem 70B
-- (*) Check whether a given term represents a multiway tree.

-- In Prolog or Lisp, one writes a predicate to check this.

-- Example in Prolog:

-- ?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
-- Yes
-- In Haskell, we define multiway trees as a datatype, as in the module Data.Tree:

-- data Tree a = Node a [Tree a]
--         deriving (Eq, Show)
-- Some example trees:

-- tree1 = Node 'a' []
 
-- tree2 = Node 'a' [Node 'b' []]
 
-- tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
-- tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
-- tree5 = Node 'a' [
--                 Node 'f' [Node 'g' []],
--                 Node 'c' [],
--                 Node 'b' [Node 'd' [], Node 'e' []]
--                 ]
-- The last is the tree illustrated above.

-- As in problem 54A, all members of this type are multiway trees; there is no use for a predicate to test them.

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

isTree :: Tree a -> Bool
isTree tree = True                 
