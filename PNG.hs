
import Util
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get (getWord16le, getWord32le, getWord32be, runGet)
import Data.Binary.Put (putWord16le, putWord32le)
import Debug.Trace

data PNGFileHeader = FileHeader {
        pCheckPNG :: Word32, -- includes high bit and PNG
        pEOL :: Word32} deriving Show

instance Binary PNGFileHeader where
    get = do
        check <- getWord32le
        eol <- getWord32le
        return FileHeader {
            pCheckPNG = check,
            pEOL = eol }
    put header = do
        putWord32le $ pCheckPNG header
        putWord32le $ pEOL header

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
  -- | trace ("prop: " ++ show (datLength nxtC) ++ " " 
  --                   ++ show (datType nxtC)) False = undefined
  | datType nxtC == pngIEND = nxtC : []
  | otherwise = nxtC : decodePNGChunks rest
  where (nxtC,rest) = decodeChunk buf

readTMP :: BL.ByteString -> [Chunk]
readTMP buf = decodePNGChunks rst
    where (_,rst) = BL.splitAt (fromIntegral sizePNGFileHeader) buf

readCritical :: BL.ByteString -> [Chunk]
readCritical = filter ((\x -> elem x criticalChunks) . datType) . readTMP

-- data PNG = PNG {
--         pngFileHeader :: PNGFileHeader,
--         pngInfoHeader :: PNGInfoHeader,
--         pngPalette :: PNGPalette,
--         imageData :: BL.ByteString }
-- readPNG :: BL.ByteString -> PNG

