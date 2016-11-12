module DCT (
    decode,
    zigzag,
    unzigzag,
    ) where

import Util (zip2D, zip2D3)
import Data.List (transpose)

shift :: Int -> [[Int]] -> [[Int]]
shift s = map (map (\x -> x - s))

-- u -> columns, v -> rows
-- x -> columns, y ->rows
-- (0,0) -> DC (constant component) coefficient
-- Remaining 63 -> AC (alternating components) coefficients
dctConversion :: [[Int]] -> [[Double]]
dctConversion g = [[transform u v | u <- [0..7]] | v <- [0..7]]
    where
        transform :: Int -> Int -> Double
        transform u v = 1/4 * norms * (sum [calc x y | x <- [0..7], y <- [0..7]])
            where
                alph u = if u == 0 then 1 / (sqrt 2) else 1
                norms = alph u * alph v
                calc x y = fromIntegral (g !! y !! x) * calc' x u * calc' y v
                calc' a b = let a' = fromIntegral a 
                                b' = fromIntegral b in
                                cos ((2 * a' + 1) * b' * pi / 16)

quantize :: [[Double]] -> [[Double]] -> [[Int]]
quantize qnt = map (map freqDim) . zip2D qnt
    where freqDim (q,g) = round (g/q)

zigzag :: [[a]] -> [a]
zigzag mat = foldr (++) [] [build x | x <- [0..16]]
    where build s = let diag = [mat !! x !! (s - x) | x <- [0..s], x < 8, s - x < 8] in
            if s `mod` 2 == 1 then diag else reverse diag

huffman :: [Int] -> (Int,[((Int,Int),Maybe Int)])
huffman b = (dc, compress 1 0 ac)
    where
        (dc:ac) = (reverse . dropWhile (== 0) . reverse) b
        compress :: Int -> Int -> [Int] -> [((Int,Int),Maybe Int)]
        compress _ _ [] = [((0,0),Nothing)]
        compress idx cnt t@(x:xs)
          | cnt == 16 && x == 0 = ((15,0),Just 0) : compress idx 0 t
          | x == 0 = compress (idx+1) (cnt+1) xs
          | otherwise = ((cnt,bitSz x), Just x) : compress (idx+1) 0 xs
          where bitSz = (+1) . floor . log2 . fromIntegral . abs
                log2 v = log v / log 2

encode :: [[Double]] -> [[Int]] -> (Int,[((Int,Int),Maybe Int)])
encode qnt = huffman . zigzag . quantize qnt . dctConversion . shift 128

-- we care about this part for decoding
unzigzag :: [a] -> [[a]]
unzigzag arr = let frnt = build arr
                   back = build $ reverse arr in
                map (\(a,b) -> a ++ tail b) (zip frnt (reverse (map reverse back)))
    where
        build as = [[locate x y as | y <- [0..7], x+y < 8] | x <- [0..7]]
        locate x y as = if s `mod` 2 == 1 then as !! (prev + diff) 
                                       else as !! (prev + s - diff)
            where s = x + y
                  prev = s * (s + 1) `div` 2
                  diff = s - y

revQuantize :: [[Double]] -> [[Int]] -> [[Double]]
revQuantize qnt = map (map freqInc) . zip2D qnt
    where freqInc (q,g) = q * (fromIntegral g)

mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

dctmat :: [[Double]]
dctmat = [[t i j | j <- [0..7]] | i <- [0..7]]
    where t i j
            | i == 0 = 1 / sqrt 8
            | otherwise = 1 / 2 * cos ((2 * j + 1) * i * pi / 16)

idct :: [[Double]]
idct = transpose dctmat

dctInverse :: [[Int]] -> [[Int]]
dctInverse f = [[round (transform x y) | x <- [0..7]] | y <- [0..7]]
    where
        transform :: Int -> Int -> Double
        transform x y = 1/4 * (sum [calc u v | u <- [0..7], v <- [0..7]])
            where
                alph u = if u == 0 then 1 / (sqrt 2) else 1
                calc u v = alph u * alph v * fromIntegral (f !! v !! u) * 
                             calc' x u * calc' y v
                calc' a b = let a' = fromIntegral a 
                                b' = fromIntegral b in
                                cos ((2 * a' + 1) * b' * pi / 16)

-- decode :: [[Double]] -> [Int] -> [[Int]]
-- decode qnt = shift (-128) . dctInverse . revQuantize qnt . unzigzag

decode :: [[Double]] -> [Int] -> [[Int]]
decode qnt arr = shift (-128) $ map (map round) d
    where m = revQuantize qnt . unzigzag $ arr
          d = mmult idct $ mmult m dctmat

