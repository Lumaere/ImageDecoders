module BMPInfoHeader (
    BMPInfoHeader (..),
    BMPInfoV3 (..),
    sizeBMPInfoHeader,
    getBaseInfo,
  ) where

import Data.Binary
import Data.Binary.Get (getWord16le, getWord32le)
import Data.Binary.Put (putWord16le, putWord32le)
import Control.Applicative

-- can extend to support other DIB versions
data BMPInfoHeader = InfoV3 BMPInfoV3
                   -- | InfoV4 BMPInfoV4
                   -- | InfoV5 BMPInfoV5
                   deriving Show

instance Binary BMPInfoHeader where
  get = 
    (do 40 <- getWord32le
        info <- get
        return $ InfoV3 info)
    <|>
    (error "Unhandled header size");

  put sub = case sub of
        InfoV3 info -> put info

getBaseInfo :: BMPInfoHeader -> BMPInfoV3
getBaseInfo header = case header of
    InfoV3 info -> info

data BMPInfoV3 = BMPInfoV3 {
        dibSize :: Word32,
        dibWidth :: Word32,
        dibHeight :: Word32,
        dibPlanes :: Word16,
        dibBitCount :: Word16,
        dibCompression :: Word32,
        dibSizeImage :: Word32,
        dibXPelsPerMeter :: Word32,
        dibYPelsPerMeter :: Word32,
        dibClrUsed :: Word32,
        dibClrImportant :: Word32} deriving Show

instance Binary BMPInfoV3 where
    get = do
        -- bSize          <- getWord32le
        bWidth         <- getWord32le
        bHeight        <- getWord32le
        bPlanes        <- getWord16le
        bBitCount      <- getWord16le
        bCompression   <- getWord32le
        bSizeImage     <- getWord32le
        bXPelsPerMeter <- getWord32le
        bYPelsPerMeter <- getWord32le
        bClrUsed       <- getWord32le
        bClrImportant  <- getWord32le
        return BMPInfoV3 {
            dibSize          = 40,
            dibWidth         = bWidth,
            dibHeight        = bHeight,
            dibPlanes        = bPlanes,
            dibBitCount      = bBitCount,
            dibCompression   = bCompression,
            dibSizeImage     = bSizeImage,
            dibXPelsPerMeter = bXPelsPerMeter,
            dibYPelsPerMeter = bYPelsPerMeter,
            dibClrUsed       = bClrUsed,
            dibClrImportant  = bClrImportant}

    put header = do
        putWord32le $ dibSize header
        putWord32le $ dibWidth header
        putWord32le $ dibHeight header
        putWord16le $ dibPlanes header
        putWord16le $ dibBitCount header
        putWord32le $ dibCompression header
        putWord32le $ dibSizeImage header
        putWord32le $ dibXPelsPerMeter header
        putWord32le $ dibYPelsPerMeter header
        putWord32le $ dibClrUsed header
        putWord32le $ dibClrImportant header

sizeBMPInfoHeader :: Int
sizeBMPInfoHeader = 40

