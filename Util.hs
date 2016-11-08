module Util (
    groupN,
    strAscii
    ) where

import Data.Char (ord)
import Data.List (foldl')

groupN :: Int -> [a] -> [[a]]
groupN n xs
 | null xs = []
 | otherwise = take n xs : groupN n (drop n xs)

strAscii :: String -> Int
strAscii = foldl' (\n c -> n * 256 + ord c) 0

