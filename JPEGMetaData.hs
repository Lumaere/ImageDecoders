module JPEGMetaData (
    JPEGFileHeader (..),
    FrameInfo (..),
    QuantTable (..),
    HuffmanTable (..),
    ScanMarker (..),
    emptyJPEGFileHeader,
    emptyFrameInfo,
    emptyScanMarker,
    ) where

import Util (getMany, putMany, groupN)
import Data.Binary
import Data.Binary.Get (getWord16be, getWord32be)
import Data.Binary.Put (putWord16be, putWord32be)
import Data.Bits ((.&.), (.|.), shift, shiftR)

data JPEGFileHeader = JPEGFileHeader {
        hAPP0 :: Word16,
        hLength :: Word16,
        hIdentifer :: Word32,
        hVer :: Word16,
        hUnits :: Word8,
        hXdensity :: Word16,
        hYdensity :: Word16,
        hThumbnailWidth :: Word8,
        hThumbnailHeight :: Word8 } deriving Show
emptyJPEGFileHeader = JPEGFileHeader {
        hAPP0 = 0, hLength = 0, hIdentifer = 0, hVer = 0, hUnits = 0,
        hXdensity = 0, hYdensity = 0, hThumbnailWidth = 0, hThumbnailHeight = 0 }

sizeJPEGFileHeader :: Int
sizeJPEGFileHeader = 18

instance Binary JPEGFileHeader where
    get = do
        _ <- getWord16be
        app0 <- getWord16be
        ben <- getWord16be
        ident <- getWord32be
        0 <- getWord8
        ver <- getWord16be
        units <- getWord8
        xdensity <- getWord16be
        ydensity <- getWord16be
        w <- getWord8
        h <- getWord8
        return JPEGFileHeader {
            hAPP0 = app0,
            hLength = ben,
            hIdentifer = ident,
            hVer = ver,
            hUnits = units,
            hXdensity = xdensity,
            hYdensity = ydensity,
            hThumbnailWidth = w,
            hThumbnailHeight = h }

    put header = do
        putWord16be $ 0xFFE0
        putWord16be $ hAPP0 header
        putWord16be $ hLength header
        putWord32be $ hIdentifer header
        putWord16be $ hVer header
        putWord8    $ hUnits header
        putWord16be $ hXdensity header
        putWord16be $ hYdensity header
        putWord8    $ hThumbnailWidth header
        putWord8    $ hThumbnailHeight header

data FrameComponent = FrameComponent {
        fcID :: Word8,
        fcVertHorz :: Word8,
        fcQuantNum :: Word8 } deriving Show
instance Binary FrameComponent where
    get = do
        id <- getWord8
        vertHorz <- getWord8
        quant <- getWord8
        return FrameComponent {
            fcID = id,
            fcVertHorz = vertHorz,
            fcQuantNum = quant }

    put header = do
        putWord8 $ fcID header
        putWord8 $ fcVertHorz header
        putWord8 $ fcQuantNum header

data FrameInfo = FrameInfo {
        sofLength :: Word16,
        sofPrecision :: Word8,
        sofHeight :: Word16,
        sofWidth :: Word16,
        sofCompNum :: Word8,
        sofComps :: [FrameComponent] } deriving Show
emptyFrameInfo = FrameInfo { sofLength = 0, sofPrecision = 0, sofHeight = 0, 
                             sofWidth = 0, sofCompNum = 0, sofComps = [] }
instance Binary FrameInfo where
    get = do
        _ <- getWord16be
        len <- getWord16be
        prec <- getWord8
        height <- getWord16be
        width <- getWord16be
        num <- getWord8
        comps <- getMany . fromIntegral $ num
        return FrameInfo {
            sofLength = len,
            sofPrecision = prec,
            sofHeight = height,
            sofWidth = width,
            sofCompNum = num,
            sofComps = comps }

    put header = do
        putWord16be $ 0xFFC0
        putWord16be $ sofLength header
        putWord8    $ sofPrecision header
        putWord16be $ sofHeight header
        putWord16be $ sofWidth header
        putWord8    $ sofCompNum header
        putMany     $ sofComps header

data QuantTable = QuantTable {
        qtLength :: Word16,
        qtNum :: Word8,
        qtPrec :: Word8,
        table :: [[Double]] } deriving Show
instance Binary QuantTable where
    get = do
        _ <- getWord16be
        len <- getWord16be
        info <- getWord8
        let prec = info .&. 0xF0
        arr <- getMany $ 64 * (fromIntegral prec + 1) :: Get [Word8]
        return QuantTable {
            qtLength = len,
            qtNum = info .&. 0x0F,
            qtPrec = info .&. 0xF0,
            table = groupN 8 . map fromIntegral $ arr }

    put qnt = do
        putWord16be $ 0xFFDB
        putWord16be $ qtLength qnt
        putWord8    $ qtNum qnt .|. qtPrec qnt
        putMany     $ (map round . foldr (++) [] . table $ qnt :: [Word8])

data HuffmanTable = HuffmanTable {
        htLength :: Word16,
        htNum :: Word8,
        htType :: Word8,
        htNumSymbols :: [Word8],
        htSymbols :: [Word8] } deriving Show
instance Binary HuffmanTable where
    get = do
        _ <- getWord16be
        len <- getWord16be
        info <- getWord8
        cnts <- getMany 16 :: Get [Word8]
        let n = fromIntegral $ sum cnts
        arr <- getMany n
        return HuffmanTable {
            htLength = len,
            htNum = info .&. 0xF,
            htType = shift info (-4),
            htNumSymbols = cnts,
            htSymbols = arr }

    put hft = do
        putWord16be $ 0xFFC4
        putWord16be $ htLength hft
        putWord8    $ shift (htType hft) 4 .|. htNum hft
        putMany     $ htNumSymbols hft
        putMany     $ htSymbols hft

data ScanMarker = ScanMarker {
        sosLength :: Word16,
        sosNumComp :: Word8,
        sosComponents :: [(Word8,Word8)] } deriving Show
emptyScanMarker = ScanMarker { sosLength = 0, sosNumComp = 0, sosComponents = [] }
instance Binary ScanMarker where
    get = do
        _ <- getWord16be
        len <- getWord16be
        num <- getWord8
        comps <- getMany $ fromIntegral num
        _ <- getWord8  -- skip 3 bytes
        _ <- getWord8 
        _ <- getWord8 
        return ScanMarker {
            sosLength = len,
            sosNumComp = num,
            sosComponents = comps }
    
    put sos = do
        putWord16be $ 0xFFDA
        putWord16be $ sosLength sos
        putWord8    $ sosNumComp sos
        putMany     $ sosComponents sos
        putWord8    $ 0 -- put 3 bytes back
        putWord8    $ 0
        putWord8    $ 0

