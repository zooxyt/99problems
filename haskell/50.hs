 -- Problem 50

-- (***) Huffman codes.

-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:

-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs

-- Example in Haskell:

-- *Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

import Data.List
import Data.Function

data Tree = Leaf { ch :: Char, freq :: Int }
          | Branch { left :: Tree, right :: Tree, freq :: Int }
          | NullTree
          deriving (Show)

huffmanTree :: [Tree] -> Tree
huffmanTree freq_tbl
  | length freq_tbl == 0 = NullTree
  | length freq_tbl == 1 = head freq_tbl
  | otherwise =
    let node0 = freq_tbl !! 0
        node1 = freq_tbl !! 1
        new_node = Branch node0 node1 (freq node0 + freq node1)
        freq_tbl_rest = drop 2 freq_tbl
        freq_tbl_merger = (takeWhile (\x->freq x < freq new_node) freq_tbl_rest) ++
                          [new_node] ++
                          (dropWhile (\x->freq x < freq new_node) freq_tbl_rest)
    in huffmanTree freq_tbl_merger

annotateTree :: Tree -> String -> [(Char, String)]
annotateTree tree code =
  case tree of
   NullTree -> []
   Leaf ch _ -> [(ch, code)]
   Branch left right _ -> (annotateTree left (code ++ "0")) ++
                          (annotateTree right (code ++ "1"))
                     
  
huffman :: [(Char, Int)] -> [(Char, String)]
huffman freq =
  let sorted_freq = map (\x->Leaf (fst x) (snd x)) $ sortBy (on compare snd) freq
      tree = huffmanTree sorted_freq
  in annotateTree tree ""
