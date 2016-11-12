module ColorFormats (
    yCbCrToRGB,
    yCbCrToRGB',
    rgbToGray,
    rgbToGray',
    ) where

yCbCrToRGB :: (Int,Int,Int) -> (Int,Int,Int)
yCbCrToRGB (y,cb,cr) = let [r,g,b] = yCbCrToRGB' [y,cb,cr] in (r,g,b)

-- assume full colour spectrum is used -> without footroom or headroom
yCbCrToRGB' :: [Int] -> [Int]
yCbCrToRGB' (y:cb:cr:[]) = map round [sum $ zipWith (*) px $ mat2!!i | i <- [0..2]]
    where px = map fromIntegral [y, cb - 128, cr - 128]
          mat = [[ 1.164 ,   0.000 ,   1.596 ],
                 [ 1.164 , - 0.392 , - 0.813 ],
                 [ 1.164 ,   2.017 ,   0.000 ]]
          mat2 = [[ 1.000 ,   0.000 ,   1.400 ],
                  [ 1.000 , - 0.343 , - 0.711 ],
                  [ 1.000 ,   1.765 ,   0.000 ]]

rgbToGray :: (Int,Int,Int) -> Int
rgbToGray (r,g,b) = rgbToGray' [r,g,b]

rgbToGray' :: [Int] -> Int
rgbToGray' t@(_:_:_:[]) = round . sum $ zipWith (*) px whts
    where px = map fromIntegral t
          whts = [0.3, 0.6, 0.11]

