
import Util (getMany, putMany, duplicateIdxs, toBits, roll, groupN, everyN)
import HuffmanTree (HuffmanTree, buildHuffmanTree, huffmanTreeLookup)
import JPEGMetaData
import qualified DCT as D (decode)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Data.Binary (decode, Word16, Word8)
import Data.Binary.Get (getWord16be, runGet)
import Data.Bits ((.&.), (.|.), shift, shiftR)
import Data.List (foldl')
import Debug.Trace

data JPEG = JPEG {
        fileHeader :: JPEGFileHeader,
        infoHeader :: FrameInfo,
        quantTables :: M.Map (Int) [[Double]],
        huffmanTrees :: M.Map (Int,Int) HuffmanTree,
        scanInfo :: ScanMarker,
        imageData :: BL.ByteString }

data Segment = Seg1 JPEGFileHeader
             | Seg2 FrameInfo
             | Seg3 QuantTable
             | Seg4 HuffmanTable 
             | Seg5 ScanMarker
             | Seg6 BL.ByteString
             deriving Show

readJPEG :: BL.ByteString -> JPEG
readJPEG buf = foldr updateJPEG initial $ readSegments buf
    where initial = JPEG {
            fileHeader = emptyJPEGFileHeader,
            infoHeader = emptyFrameInfo,
            huffmanTrees = M.empty,
            quantTables = M.empty,
            scanInfo = emptyScanMarker,
            imageData = BL.empty }

updateJPEG :: Segment -> JPEG -> JPEG
updateJPEG seg state = case seg of
    Seg1 x -> state { fileHeader = x }
    Seg2 x -> state { infoHeader = x }
    Seg3 x -> let n = fromIntegral $ qtNum x 
              in state { quantTables = M.insert n (table x) (quantTables state) }
    Seg4 x -> let n = fromIntegral $ htNum x
                  t = fromIntegral $ htType x
                  lvls = zip (map fromIntegral $ duplicateIdxs (htNumSymbols x)) 
                             (map fromIntegral $ htSymbols x)
              in state { huffmanTrees = M.insert (t,n) (buildHuffmanTree lvls) (huffmanTrees state) }
    Seg5 x -> state { scanInfo = x }
    Seg6 x -> state { imageData = x }
    

readSegments :: BL.ByteString -> [Segment]
readSegments buf
  | marker == 0xFFD8 = let x = decode buf in Seg1 x : readSegments (BL.drop 20 buf)
  | marker == 0xFFC0 = let x = decode buf in Seg2 x : readSegments (BL.drop (2 + fromIntegral (sofLength x)) buf)
  | marker == 0xFFDB = let x = decode buf in Seg3 x : readSegments (BL.drop (2 + fromIntegral (qtLength x)) buf)
  | marker == 0xFFC4 = let x = decode buf in Seg4 x : readSegments (BL.drop (2 + fromIntegral (htLength x)) buf)
  | marker == 0xFFDA = let x = decode buf in Seg5 x : Seg6 (BL.drop (2 + fromIntegral (sosLength x)) buf) : []
  | otherwise = error "Unhandled segment type"
  where marker = runGet getWord16be buf

dcValue :: [Word8] -> Int
dcValue []  = 0
dcValue arr = if head arr == 1 then roll arr 
                               else negate . roll . invert $ arr
    where
        invert = map (\x -> 1 - x)

decodeDCValue :: HuffmanTree -> [Word8] -> (Int,[Word8])
decodeDCValue hf xs = let (sz, rst) = huffmanTreeLookup hf xs 
                          y = fromIntegral sz 
                     in (dcValue $ take y rst, drop y rst)

decodeACValues :: HuffmanTree -> [Word8] -> ([Int], [Word8])
decodeACValues hf xs = let (ac, rest) = rec xs [] 63
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

