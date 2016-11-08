module Util (
    groupN,
    ) where

groupN :: Int -> [a] -> [[a]]
groupN n xs
 | null xs = []
 | otherwise = take n xs : groupN n (drop n xs)

