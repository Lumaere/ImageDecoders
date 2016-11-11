
import Util
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get (getWord16be, getWord32be, runGet)
import Data.Binary.Put (putWord16be, putWord32be)

data JPEGFileHeader = JPEGFileHeader {
        hSOI :: Word16,
        hAPP0 :: Word16,
        hLength :: Word16,
        hIdentifer :: Word32,
        hVer :: Word16,
        hUnits :: Word8,
        hXdensity :: Word16,
        hYdensity :: Word16,
        hThumbnailWidth :: Word8,
        hThumbnailHeight :: Word8 } deriving Show

sizeJPEGFileHeader :: Int
sizeJPEGFileHeader = 20

instance Binary JPEGFileHeader where
    get = do
        soi <- getWord16be
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
            hSOI = soi,
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
        putWord16be $ hSOI header
        putWord16be $ hAPP0 header
        putWord16be $ hLength header
        putWord32be $ hIdentifer header
        putWord16be $ hVer header
        putWord8    $ hUnits header
        putWord16be $ hXdensity header
        putWord16be $ hYdensity header
        putWord8    $ hThumbnailWidth header
        putWord8    $ hThumbnailHeight header

-- SOI       0xFF, 0xD8              none            Start Of Image
-- SOF0      0xFF, 0xC0              variable size   Start Of Frame (baseline DCT)       Indicates that this is a baseline DCT-based JPEG, and specifies the width, height, number of components, and component subsampling (e.g., 4:2:0).
-- SOF2      0xFF, 0xC2              variable size   Start Of Frame (progressive DCT)    Indicates that this is a progressive DCT-based JPEG, and specifies the width, height, number of components, and component subsampling (e.g., 4:2:0).
-- DHT       0xFF, 0xC4              variable size   Define Huffman Table(s)             Specifies one or more Huffman tables.
-- DQT       0xFF, 0xDB              variable size   Define Quantization Table(s)        Specifies one or more quantization tables.
-- DRI       0xFF, 0xDD              4 bytes         Define Restart Interval             Specifies the interval between RSTn markers, in macroblocks. This marker is followed by two bytes indicating the fixed size so it can be treated like any other variable size segment.
-- SOS       0xFF, 0xDA              variable size   Start Of Scan                       Begins a top-to-bottom scan of the image. In baseline DCT JPEG images, there is generally a single scan. Progressive DCT JPEG images usually contain multiple scans. This marker specifies which slice of data it will contain, and is immediately followed by entropy-coded data.
-- RSTn      0xFF, 0xDn (n=0..7)     none            Restart                             Inserted every r macroblocks, where r is the restart interval set by a DRI marker. Not used if there was no DRI marker. The low three bits of the marker code cycle in value from 0 to 7.
-- APPn      0xFF, 0xEn              variable size   Application-specific                For example, an Exif JPEG file uses an APP1 marker to store metadata, laid out in a structure based closely on TIFF.
-- COM       0xFF, 0xFE              variable size   Comment                             Contains a text comment.
-- EOI       0xFF, 0xD9              none            End Of Image

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

-- Finally figured out how to do serialization of lists
getMany :: Binary a => Int -> Get [a]
getMany n = rec [] n
    where 
        rec xs 0 = return $ reverse xs
        rec xs i = do x <- get
                      x `seq` rec (x:xs) (i-1)

putMany :: Binary a => [a] -> Put
putMany = mapM_ put

data FrameInfo = FrameInfo {
        sofLength :: Word16,
        sofPrecision :: Word8,
        sofHeight :: Word16,
        sofWidth :: Word16,
        sofCompNum :: Word8,
        sofComps :: [FrameComponent] } deriving Show
instance Binary FrameInfo where
    get = do
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
        putWord16be $ sofLength header
        putWord8    $ sofPrecision header
        putWord16be $ sofHeight header
        putWord16be $ sofWidth header
        putWord8    $ sofCompNum header
        putMany     $ sofComps header

        
-- decodeSOF0 :: BL.ByteString -> FrameInfo


decodeQuant :: BL.ByteString -> [[Double]]
decodeQuant buf = groupN 8 . map fromIntegral . BL.unpack $ table
    where 
        size = fromIntegral . runGet getWord16be $ bufSize
        num = fromIntegral . runGet getWord8 $ bufNum
        (bufSize,rest1) = BL.splitAt 2 . BL.drop 2 $ buf
        (bufNum,table) = BL.splitAt 1 rest1

tmp1 :: BL.ByteString -> FrameInfo
tmp1 buf = decode . BL.drop 2 $ rst3
    where
        (_,rst) = BL.splitAt (fromIntegral sizeJPEGFileHeader) buf
        (q1,rst2) = BL.splitAt 69 rst
        (q2,rst3) = BL.splitAt 69 rst2

