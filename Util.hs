module Util (
    groupN,
    everyN,
    strAscii,
    getMany,
    putMany,
    duplicateVals,
    duplicateIdxs,
    roll,
    toTuple,
    zip2D,
    zip2D3,
    ) where

import Data.Char (ord)
import Data.List (foldl')
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Lazy as BL

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

duplicateVals :: (Integral a) => [a] -> [a]
duplicateVals = foldr ((++) . (\x -> take (fromIntegral x) $ repeat x)) []

duplicateIdxs :: (Integral a) => [a] -> [a]
duplicateIdxs vals = foldr dup [] (zip [1..] vals)
    where dup (i,x) ls = take (fromIntegral x) (repeat i) ++ ls

roll :: [Word8] -> Int
roll = foldl' (\b a -> b `shift` 1 .|. fromIntegral a) 0

toTuple :: [a] -> (a,a,a)
toTuple (g:h:c:[]) = (g,h,c)
toTuple _ = error "Bad tuple call"

zip2D :: [[a]] -> [[b]] -> [[(a,b)]]
zip2D xss yss = map (\(a,b) -> zip a b) (zip xss yss)

zip2D3 :: [[a]] -> [[b]] -> [[c]] -> [[(a,b,c)]]
zip2D3 xss yss zss = map (\(a,b,c) -> zip3 a b c) (zip3 xss yss zss)

