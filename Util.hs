module Util (
    groupN,
    everyN,
    strAscii,
    getMany,
    putMany,
    ) where

import Data.Char (ord)
import Data.List (foldl')
import Data.Binary

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

-- Finally figured out how to do serialization of lists
getMany :: Binary a => Int -> Get [a]
getMany n = rec [] n
    where 
        rec xs 0 = return $ reverse xs
        rec xs i = do x <- get
                      x `seq` rec (x:xs) (i-1)

putMany :: Binary a => [a] -> Put
putMany = mapM_ put


