module HuffmanTree (
    HuffmanTree,
    buildHuffmanTree,
    huffmanTreeLookup,
    ) where   

import Data.Word (Word16, Word8)
import Data.Maybe (fromJust, isNothing)

data HuffmanTree = Leaf (Maybe Word16)
                 | Branch HuffmanTree HuffmanTree
                 deriving Show

buildHuffmanTree :: [(Word16,Word16)] -> HuffmanTree
buildHuffmanTree arr = let (h,_) = buildHT 0 arr in h
    where buildHT :: Int -> [(Word16,Word16)] -> (HuffmanTree,[(Word16,Word16)])
          buildHT _ [] = (Leaf Nothing, [])
          buildHT d ys@(x:xs) = 
              if fst x == fromIntegral d then (Leaf . Just . snd $ x, xs) 
                            else let (l,rst) = buildHT (d+1) ys 
                                     (r,rst2) = buildHT (d+1) rst 
                                 in (Branch l r, rst2)

huffmanTreeLookup :: HuffmanTree -> [Word8] -> (Word16, [Word8])
huffmanTreeLookup (Leaf x) xs
  | isNothing x = error "Unhandled data stream" 
  | otherwise   = (fromJust x, xs)
huffmanTreeLookup (Branch l r) (x:xs) = if x == 0 then huffmanTreeLookup l xs 
                                                  else huffmanTreeLookup r xs
huffmanTreeLookup _ [] = error "Corrupted data stream"



