module Util (
    groupN,
    everyN,
    strAscii,
    ) where

import Data.Char (ord)
import Data.List (foldl')

groupN :: Int -> [a] -> [[a]]
groupN n xs
 | null xs = []
 | otherwise = take n xs : groupN n (drop n xs)

everyN :: Int -> [a] -> [a]
everyN n xs = case drop (n-1) xs of
                (y:ys) -> y : everyN n ys
                [] -> []

strAscii :: String -> Int
strAscii = foldl' (\n c -> n * 256 + ord c) 0

