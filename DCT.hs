import Data (gray1)

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

quantMatrix :: [[Double]]
quantMatrix = [[16, 11, 10, 16, 24, 40, 51, 61],
               [12, 12, 14, 19, 26, 58, 60, 55],
               [14, 13, 16, 24, 40, 57, 69, 56],
               [14, 17, 22, 29, 51, 87, 80, 62],
               [18, 22, 37, 56, 68, 109, 103, 77],
               [24, 35, 55, 64, 81, 104, 113, 92],
               [49, 64, 78, 87, 103, 121, 130, 101],
               [72, 92, 95, 98, 112, 100, 103, 99]]

zip2D :: [[a]] -> [[b]] -> [[(a,b)]]
zip2D xss yss = map (\(a,b) -> zip a b) (zip xss yss)

quantize :: [[Double]] -> [[Int]]
quantize = map (map freqDim) . zip2D quantMatrix
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

encode :: [[Int]] -> (Int,[((Int,Int),Maybe Int)])
encode = huffman . zigzag . quantize . dctConversion . shift 128

revHuffman :: (Int,[((Int,Int),Maybe Int)]) -> [Int]
revHuffman (dc,xs) = let frnt = dc : expand xs in frnt ++ take (64 - length frnt) (repeat 0)
    where
        expand :: [((Int,Int),Maybe Int)] -> [Int]
        expand [(_,Nothing)] = []
        expand (((len,_),(Just amp)):ys) = take len (repeat 0) ++ [amp] ++ expand ys

-- TODO: finish implementing this function
unzigzag :: [Int] -> [[Int]]
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

revQuantize :: [[Int]] -> [[Int]]
revQuantize = map (map freqInc) . zip2D quantMatrix
    where freqInc (q,g) = round q * g -- q should round to correct int

dctInverse :: [[Int]] -> [[Int]]
dctInverse f = [[round (transform x y) | x <- [0..7]] | y <- [0..7]]
    where
        transform :: Int -> Int -> Double
        transform x y = 1/4 * (sum [calc u v | u <- [0..7], v <- [0..7]])
            where
                alph u = if u == 0 then 1 / (sqrt 2) else 1
                calc u v = alph u * alph v * fromIntegral (f !! v !! u) * calc' x u * calc' y v
                calc' a b = let a' = fromIntegral a 
                                b' = fromIntegral b in
                                cos ((2 * a' + 1) * b' * pi / 16)

decode :: (Int,[((Int,Int),Maybe Int)]) -> [[Int]]
decode = shift (-128) . dctInverse . revQuantize . unzigzag . revHuffman

avgError :: [[Int]] -> [[Int]] -> Double
avgError xss yss = fromIntegral tot / 64
    where diff = map (map (\(a,b) -> a-b)) (zip2D xss yss)
          tot = sum $ map abs $ foldr (++) [] diff

