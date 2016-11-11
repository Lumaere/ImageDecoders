
import Util (getMany, putMany, duplicateIdxs, toBits, roll, groupN, everyN)
import HuffmanTree (HuffmanTree, buildHuffmanTree, huffmanTreeLookup)
import qualified DCT as D (decode)
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get (getWord16be, getWord32be, runGet)
import Data.Binary.Put (putWord16be, putWord32be)
import Data.Bits ((.&.), (.|.), shift, shiftR)
import Data.List (foldl')
import Debug.Trace

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
        table :: [[Word8]] } deriving Show
instance Binary QuantTable where
    get = do
        _ <- getWord16be
        len <- getWord16be
        info <- getWord8
        let prec = info .&. 0xF0
        arr <- getMany $ 64 * (fromIntegral prec + 1)
        return QuantTable {
            qtLength = len,
            qtNum = info .&. 0x0F,
            qtPrec = info .&. 0xF0,
            table = groupN 8 arr }

    put qnt = do
        putWord16be $ 0xFFDB
        putWord16be $ qtLength qnt
        putWord8    $ qtNum qnt .|. qtPrec qnt
        putMany     $ foldr (++) [] . table $ qnt

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

data JPEG = JPEG {
        fileHeader :: JPEGFileHeader,
        infoHeader :: FrameInfo,
        quantTables :: [QuantTable],
        huffmanTables :: [[HuffmanTable]],
        scanInfo :: ScanMarker,
        imageData :: BL.ByteString }

data Segment = Seg1 JPEGFileHeader
             | Seg2 FrameInfo
             | Seg3 QuantTable
             | Seg4 HuffmanTable 
             | Seg5 ScanMarker
             | Seg6 BL.ByteString
             deriving Show

readTmp :: BL.ByteString -> [Segment]
-- readTmp empty = []
readTmp buf
  | marker == 0xFFD8 = let x = decode buf in Seg1 x : readTmp (BL.drop 20 buf)
  | marker == 0xFFC0 = let x = decode buf in Seg2 x : readTmp (BL.drop (2 + fromIntegral (sofLength x)) buf)
  | marker == 0xFFDB = let x = decode buf in Seg3 x : readTmp (BL.drop (2 + fromIntegral (qtLength x)) buf)
  | marker == 0xFFC4 = let x = decode buf in Seg4 x : readTmp (BL.drop (2 + fromIntegral (htLength x)) buf)
  | marker == 0xFFDA = let x = decode buf in Seg5 x : Seg6 (BL.drop (2 + fromIntegral (sosLength x)) buf) : []
  | otherwise = error "Unhandled segment type"
  where marker = runGet getWord16be buf

tmpData :: BL.ByteString -> BL.ByteString
tmpData buf = BL.take (BL.length xs - 2) xs
    where xs = BL.drop 0x193 buf

dcValue :: [Word8] -> Int
dcValue []  = 0
dcValue arr = if head arr == 1 then roll arr 
                               else negate . roll . invert $ arr
    where
        invert = map (\x -> 1 - x)

parseDCValue :: HuffmanTree -> [Word8] -> (Int,[Word8])
parseDCValue hf xs = let (sz, rst) = huffmanTreeLookup hf xs 
                         y = fromIntegral sz 
                     in (dcValue $ take y rst, drop y rst)

parseACValues :: HuffmanTree -> [Word8] -> ([Int], [Word8])
parseACValues hf xs = let (ac, rest) = rec xs [] 63
                      in (reverse $ foldr (++) [] ac, rest)
    where
        rec :: [Word8] -> [[Int]] -> Int -> ([[Int]], [Word8])
        rec ys acc 0 = (acc, ys)
        rec ys acc n
          | run == 0 && sz == 0 = ((take n $ repeat 0) : acc, rst)
          | run == 15 && sz == 0 = rec rst (take 16 $ repeat 0 : acc) (n - 16)
          | otherwise = rec cont ([roll numBits] : (take run $ repeat 0) : acc) (n - (run + 1))
          where (inf, rst) = huffmanTreeLookup hf ys
                (run, sz) = (fromIntegral $ inf `shift` (-4), fromIntegral $ inf .&. 0xF)
                (numBits, cont) = splitAt sz rst

cs = [0,1,5,1,1,1,1,0,0,0,0,0,0,0,0,0] :: [Word16]
hs = [2,0,1,3,4,5,6,7,8,9] :: [Word16]
hf = buildHuffmanTree $ zip (duplicateIdxs cs) hs

qnt = [[5,3,4,4,4,3,5,4],[4,4,5,5,5,6,7,12],[8,7,7,7,7,15,11,11],[9,12,17,15,18,18,17,15],[17,17,19,22,28,23,19,20],[26,21,17,17,24,33,24,26],[29,29,31,31,31,19,23,34],[36,34,30,36,28,30,31,30]] :: [[Double]]

cs2 = [0,1,3,2,3,5,6,3,4,7,5,4,5,9,9,0] :: [Word16]
hs2 = [1,0,2,3,4,17,5,18,33,6,19,49,65,81,7,34,97,113,129,145,20,50,161,35,66,82,177,83,98,114,130,146,193,209,8,21,51,225,240,36,67,148,162,22,84,99,147,241,23,37,52,68,69,86,115,116,132,24,54,70,100,131,164,178,194,210] :: [Word16]
hf2 = buildHuffmanTree $ zip (duplicateIdxs cs2) hs2

