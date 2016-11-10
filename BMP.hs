module BMP (
    BMPFileHeader (..),
    BMP (..),
    readBMP,
    writeBMP,
    dimensionsBMP
    ) where

import Util
import BMPFileHeader
import BMPInfoHeader
import qualified Data.ByteString.Lazy as BL
import Data.Binary (Word8, decode, encode)
import Data.Maybe (isNothing, fromJust)

type Pixel = (Word8, Word8, Word8)
data ColorTable = ColorTable {pixels :: [Pixel]} deriving Show

decodeCT :: BL.ByteString -> ColorTable
decodeCT = ColorTable . map (\(r:g:b:_) -> (r,g,b)) . groupN 4 . BL.unpack
encodeCT :: ColorTable -> BL.ByteString
encodeCT = BL.pack . foldr (++) [] . map (\(r,g,b) -> r:g:b:0:[]) . pixels

data BMP = BMP {
        bmpFileHeader :: BMPFileHeader,
        bmpInfoHeader :: BMPInfoHeader,
        bmpColorTable :: Maybe ColorTable,
        bmpImageData :: BL.ByteString} deriving Show

readBMP :: BL.ByteString -> BMP
readBMP buf = BMP {
        bmpFileHeader = decode bufFileHead,
        bmpInfoHeader = infoHead,
        bmpColorTable = colorTable,
        bmpImageData = imageDat}
    where
        (bufFileHead,rest1) = BL.splitAt (fromIntegral sizeBMPFileHeader) buf
        (bufInfoHead,rest2) = BL.splitAt (fromIntegral sizeBMPInfoHeader) rest1
        infoHead = decode bufInfoHead
        tableSize = (fromIntegral . (*4) . dibClrUsed . getBaseInfo) infoHead
        bitCount = dibBitCount $ getBaseInfo infoHead
        colorTable = if bitCount == 8 
                     then Just $ decodeCT (BL.take tableSize rest2) else Nothing
        imageDat = if bitCount == 8 then BL.drop tableSize rest2 else rest2

writeBMP :: BMP -> BL.ByteString
writeBMP bmp = BL.append (BL.append header table) $ bmpImageData bmp
    where
        header = BL.append (encode (bmpFileHeader bmp)) (encode (bmpInfoHeader bmp))
        table = if isNothing $ bmpColorTable bmp 
                then BL.empty else encodeCT $ fromJust $ bmpColorTable bmp

dimensionsBMP :: BMP -> (Int, Int)
dimensionsBMP bmp = (fromIntegral $ dibWidth info, fromIntegral $ dibHeight info)
    where info = getBaseInfo $ bmpInfoHeader bmp
        
