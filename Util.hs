module Util (
    groupN,
    everyN,
    strAscii,
    getMany,
    putMany,
    duplicateVals,
    duplicateIdxs,
    toBits,
    roll,
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

toBits :: BL.ByteString -> [Word8]
toBits = foldr (\x acc -> split x ++ acc) [] . removeStuffed . BL.unpack
    where 
        split x = [shift x y .&. 0x01 | y <- [-7..0]]
        removeStuffed [] = []
        removeStuffed [x] = [x]
        removeStuffed (x:y:xs)
          | x == 0xff = if y /= 0x00 then error "Bad stream"
                                     else x : removeStuffed xs
          | otherwise = removeStuffed (y:xs)

roll :: [Word8] -> Int
roll = foldl' (\b a -> b `shift` 1 .|. fromIntegral a) 0


