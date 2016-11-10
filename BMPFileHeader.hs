module BMPFileHeader (
    BMPFileHeader (..),
    sizeBMPFileHeader,
  ) where

import Data.Binary
import Data.Binary.Get (getWord16le, getWord32le)
import Data.Binary.Put (putWord16le, putWord32le)

data BMPFileHeader = FileHeader {
        bfType :: Word16,
        bfSize :: Word32,
        bfReserved1 :: Word16,
        bfReserved2 :: Word16,
        bfOffBits :: Word32} deriving Show

instance Binary BMPFileHeader where
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

sizeBMPFileHeader :: Int
sizeBMPFileHeader = 14

