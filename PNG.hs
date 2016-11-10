module PNG (
    PNGFileHeader (..),
    IHDR (..),
    PNG (..),
    readPNG,
    reconstruct,
    getScanlines,
    ) where

import Util
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.Zlib as Z
import Data.Binary
import Data.Binary.Get (getWord16be, getWord32be, runGet)
import Data.Binary.Put (putWord16be, putWord32be)
import Data.List (foldl')
import Debug.Trace

data PNGFileHeader = FileHeader {
        pCheckPNG :: Word32, -- includes high bit and PNG
        pEOL :: Word32} deriving Show

instance Binary PNGFileHeader where
    get = do
        check <- getWord32be
        eol <- getWord32be
        return FileHeader {
            pCheckPNG = check,
            pEOL = eol }
    put header = do
        putWord32be $ pCheckPNG header
        putWord32be $ pEOL header

data IHDR = IHDR {
        width :: Word32,
        height :: Word32,
        bitDepth :: Word8,
        colourType :: Word8,
        pcompression :: Word8,
        pfilter :: Word8,
        pinterlace :: Word8 } deriving Show


instance Binary IHDR where
    get = do
        width <- getWord32be
        height <- getWord32be
        bitDepth <- getWord8
        colourType <- getWord8
        pcompression <- getWord8
        pfilter <- getWord8
        pinterlace <- getWord8
        return IHDR {
            width = width,
            height = height,
            bitDepth = bitDepth,
            colourType = colourType,
            pcompression = pcompression,
            pfilter = pfilter,
            pinterlace = pinterlace }
    put header = do
        putWord32be $ width header
        putWord32be $ height header
        putWord8 $ bitDepth header
        putWord8 $ colourType header
        putWord8 $ pcompression header
        putWord8 $ pfilter header
        putWord8 $ pinterlace header

sizePNGFileHeader :: Int
sizePNGFileHeader = 8

data Chunk = Chunk {
        datLength :: Word32,
        datType :: Word32,
        chunkData :: BL.ByteString,
        checksum :: Word32 } deriving Show

decodeChunk :: BL.ByteString -> (Chunk, BL.ByteString)
decodeChunk buf = (Chunk {
        datLength = len,
        datType = runGet getWord32be bufType,
        chunkData = bufData,
        checksum = runGet getWord32be bufCheck },restOut)
    where 
        len = runGet getWord32be bufLen
        (bufLen,tmp1) = BL.splitAt 4 buf
        (bufType,tmp2) = BL.splitAt 4 tmp1
        (bufData,tmp3) = BL.splitAt (fromIntegral len) tmp2
        (bufCheck,restOut) = BL.splitAt 4 tmp3

pngIHDR = fromIntegral . strAscii $ "IHDR"
pngPLTE = fromIntegral . strAscii $ "PLTE"
pngIDAT = fromIntegral . strAscii $ "IDAT"
pngIEND = fromIntegral . strAscii $ "IEND"

criticalChunks :: [Word32]
criticalChunks = [pngIHDR, pngPLTE, pngIDAT, pngIEND]


decodePNGChunks :: BL.ByteString -> [Chunk]
decodePNGChunks buf
  | datType nxtC == pngIEND = nxtC : []
  | otherwise = nxtC : decodePNGChunks rest
  where (nxtC,rest) = decodeChunk buf

-- x	the byte being filtered;
-- a	the byte corresponding to x in the pixel immediately before the pixel 
--      containing x (or the byte immediately before x, when the bit depth is 
--      less than 8);
-- b	the byte corresponding to x in the previous scanline;
-- c	the byte corresponding to b in the pixel immediately before the pixel 
--      containing b (or the byte immediately before b, when the bit depth is 
--      less than 8).

-- 0	None	Filt(x) = Orig(x)	
--              Recon(x) = Filt(x)
-- 1	Sub	    Filt(x) = Orig(x) - Orig(a)	
--              Recon(x) = Filt(x) + Recon(a)
-- 2	Up	    Filt(x) = Orig(x) - Orig(b)	
--              Recon(x) = Filt(x) + Recon(b)
-- 3	Average	Filt(x) = Orig(x) - floor((Orig(a) + Orig(b)) / 2)	
--              Recon(x) = Filt(x) + floor((Recon(a) + Recon(b)) / 2)
-- 4	Paeth	Filt(x) = Orig(x) - PaethPredictor(Orig(a), Orig(b), Orig(c))	
--              Recon(x) = Filt(x) + PaethPredictor(Recon(a), Recon(b), Recon(c))
-- assume colourType 2 right now
reconstruct :: [Word8] -> [[Word8]] -> [[Word8]]
reconstruct ms wss = reverse $ foldl' (flip defilt) [] $ zip ms wss
    where
        defilt :: (Word8,[Word8]) -> [[Word8]] -> [[Word8]]
        defilt (0,c) rs = c:rs
        defilt (1,c) rs = let r = take 3 c ++ zipWith (+) (drop 3 c) r in r:rs
        defilt (2,c) rs = zipWith (+) c (head rs) : rs
        defilt (3,c) rs = r:rs
            where 
                r = front ++ back
                (f,b) = splitAt 3 c
                front = zipWith (+) f $ zipWith avg [0,0,0] (take 3 (head rs))
                back = zipWith (+) b $ zipWith avg r (drop 3 (head rs))
                avg :: Word8 -> Word8 -> Word8
                avg a b = fromIntegral $ (a' + b') `div` 2
                    where a' = fromIntegral a :: Int
                          b' = fromIntegral b :: Int
        defilt (4,c) rs = r:rs
            where
                r = front ++ back
                (f,b) = splitAt 3 c
                front = zipWith (+) f $ 
                            zipWith3 paeth [0,0,0] (take 3 (head rs)) [0,0,0]
                back = zipWith (+) b $ zipWith3 paeth r (drop 3 (head rs)) (head rs)
                paeth :: Word8 -> Word8 -> Word8 -> Word8
                paeth a b c
                  | pa <= pb && pa <= pc = a
                  | pb <= pc             = b
                  | otherwise            = c
                  where (a':b':c':[]) = map fromIntegral [a,b,c]
                        (pa,pb,pc) = (abs $ p-a', abs $ p-b', abs $ p-c')
                        p = a' + b' - c'
        defilt (_,_) _ = error "Unhandled filter type"

getScanlines :: PNG -> ([Word8],[[Word8]])
getScanlines xs = (frst,scnd)
    where
        w = fromIntegral . (+1) . (*3) . width . pngInfoHeader $ xs
        h = fromIntegral . height . pngInfoHeader $ xs
        frst = [BL.head $ BL.drop (w*x) $ pngImageData xs | x <- [0..h-1]]
        scnd = [BL.unpack . BL.tail . BL.take w . BL.drop (w*x) $ pngImageData xs | x <- [0..h-1]]

data PNG = PNG {
        pngFileHeader :: PNGFileHeader,
        pngInfoHeader :: IHDR,
        pngImageData :: BL.ByteString }

-- deinterlace :: IHDR -> BL.ByteString -> BL.ByteString
-- deinterlace = 

readPNG :: BL.ByteString -> PNG
readPNG buf = PNG {
        pngFileHeader = decode fileHead,
        pngInfoHeader = decode $ chunkData $ chunks!!0, -- IHDR is first
        pngImageData = Z.decompress . BL.concat . map chunkData . 
                        filter ((== pngIDAT) . datType) $ chunks }
    where
        (fileHead,rest) = BL.splitAt (fromIntegral sizePNGFileHeader) buf
        chunks = decodePNGChunks rest

