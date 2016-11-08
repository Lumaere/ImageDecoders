module BMP (
    FileHeader (..),
    InfoHeader (..),
    BMP (..),
    readBMP,
    writeBMP,
    dimensionsBMP
    ) where

import Util
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get (getWord16le, getWord32le)
import Data.Binary.Put (putWord16le, putWord32le)
import Data.Maybe (isNothing, fromJust)
import Data.Array

data FileHeader = FileHeader {
        bfType :: Word16,
        bfSize :: Word32,
        bfReserved1 :: Word16,
        bfReserved2 :: Word16,
        bfOffBits :: Word32} deriving Show

instance Binary FileHeader where
    get = do
        btype <- getWord16le
        size <- getWord32le
        res1 <- getWord16le
        res2 <- getWord16le
        offset <- getWord32le
        return FileHeader {
            bfType = btype,
            bfSize = size,
            bfReserved1 = res1,
            bfReserved2 = res2,
            bfOffBits = offset}
    put header = do
        putWord16le $ bfType header
        putWord32le $ bfSize header
        putWord16le $ bfReserved1 header
        putWord16le $ bfReserved2 header
        putWord32le $ bfOffBits header

sizeFileHeader :: Int
sizeFileHeader = 14

data InfoHeader = InfoHeader {
        biSize :: Word32,
        biWidth :: Word32,
        biHeight :: Word32,
        biPlanes :: Word16,
        biBitCount :: Word16,
        biCompression :: Word32,
        biSizeImage :: Word32,
        biXPelsPerMeter :: Word32,
        biYPelsPerMeter :: Word32,
        biClrUsed :: Word32,
        biClrImportant :: Word32} deriving Show

instance Binary InfoHeader where
    get = do
        bSize <- getWord32le
        bWidth <- getWord32le
        bHeight <- getWord32le
        bPlanes <- getWord16le
        bBitCount <- getWord16le
        bCompression <- getWord32le
        bSizeImage <- getWord32le
        bXPelsPerMeter <- getWord32le
        bYPelsPerMeter <- getWord32le
        bClrUsed <- getWord32le
        bClrImportant <- getWord32le
        return InfoHeader {
            biSize = bSize,
            biWidth = bWidth,
            biHeight = bHeight,
            biPlanes = bPlanes,
            biBitCount = bBitCount,
            biCompression = bCompression,
            biSizeImage = bSizeImage,
            biXPelsPerMeter = bXPelsPerMeter,
            biYPelsPerMeter = bYPelsPerMeter,
            biClrUsed = bClrUsed,
            biClrImportant = bClrImportant}
    put header = do
        putWord32le $ biSize header
        putWord32le $ biWidth header
        putWord32le $ biHeight header
        putWord16le $ biPlanes header
        putWord16le $ biBitCount header
        putWord32le $ biCompression header
        putWord32le $ biSizeImage header
        putWord32le $ biXPelsPerMeter header
        putWord32le $ biYPelsPerMeter header
        putWord32le $ biClrUsed header
        putWord32le $ biClrImportant header

sizeInfoHeader :: Int
sizeInfoHeader = 40

type Pixel = (Word8, Word8, Word8)
data ColorTable = ColorTable {pixels :: [Pixel]} deriving Show

decodeCT :: BL.ByteString -> ColorTable
decodeCT = ColorTable . map (\(r:g:b:_) -> (r,g,b)) . groupN 4 . BL.unpack
encodeCT :: ColorTable -> BL.ByteString
encodeCT = BL.pack . foldr (++) [] . map (\(r,g,b) -> r:g:b:0:[]) . pixels

data BMP = BMP {
        bmpFileHeader :: FileHeader,
        bmpInfoHeader :: InfoHeader,
        bmpColorTable :: Maybe ColorTable,
        imageData :: BL.ByteString} deriving Show

readBMP :: BL.ByteString -> BMP
readBMP buf = BMP {
        bmpFileHeader = decode bufFileHead,
        bmpInfoHeader = infoHead,
        bmpColorTable = colorTable,
        imageData = imageDat}
    where
        (bufFileHead,rest1) = BL.splitAt (fromIntegral sizeFileHeader) buf
        (bufInfoHead,rest2) = BL.splitAt (fromIntegral sizeInfoHeader) rest1
        infoHead = decode bufInfoHead
        tableSize = (fromIntegral . (*4) . biClrUsed) infoHead
        colorTable = if biBitCount infoHead == 8 
                     then Just $ decodeCT (BL.take tableSize rest2) else Nothing
        imageDat = if biBitCount infoHead == 8 
                   then BL.drop tableSize rest2 else rest2

writeBMP :: BMP -> BL.ByteString
writeBMP bmp = BL.append (BL.append header table) $ imageData bmp
    where
        header = BL.append (encode (bmpFileHeader bmp)) (encode (bmpInfoHeader bmp))
        table = if isNothing $ bmpColorTable bmp 
                then BL.empty else encodeCT $ fromJust $ bmpColorTable bmp

dimensionsBMP :: BMP -> (Int, Int)
dimensionsBMP bmp = (fromIntegral $ biWidth info, fromIntegral $ biHeight info)
    where info = bmpInfoHeader bmp
        
