
-- temporrary imports
import Data.Array
import ColorFormats

import Util (groupN, duplicateIdxs, roll, toTuple, zip2D3)
import HuffmanTree (HuffmanTree, buildHuffmanTree, huffmanTreeLookup)
import JPEGMetaData
import qualified DCT as D (decode)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Data.Binary (decode, Word16, Word8)
import Data.Binary.Get (getWord16be, runGet)
import Data.Bits ((.&.), (.|.), shift, shiftR)
import Data.Maybe (fromJust)
import GHC.Int (Int64)
import Control.DeepSeq (deepseq)
import Debug.Trace

toBits :: BL.ByteString -> [Word8]
toBits = foldr (++) [] . map (\x -> split x) . removeStuffed . BL.unpack
    where 
        split x = [shift x y .&. 0x01 | y <- [-7..0]]
        removeStuffed :: [Word8] -> [Word8]
        removeStuffed [] = []
        removeStuffed [x] = [x]
        removeStuffed (x:y:xs)
          | x == 0xff && y == 0x00 = x : removeStuffed xs
          | x == 0xff = if y == 0xD9 then [] else error "Bad data stream"
          | otherwise = x : removeStuffed (y:xs)

data JPEG = JPEG {
        fileHeader :: JPEGFileHeader,
        infoHeader :: FrameInfo,
        quantTables :: M.Map (Int) [[Double]],
        huffmanTrees :: M.Map (Int,Int) HuffmanTree,
        scanInfo :: ScanMarker,
        imageData :: BL.ByteString }

data Segment = Seg1 JPEGFileHeader
             | Seg2 FrameInfo
             | Seg3 QuantTables
             | Seg4 HuffmanTables
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

insertQuant :: M.Map Int [[Double]] -> [QuantTable] -> M.Map Int [[Double]]
insertQuant mp [] = mp
insertQuant mp (x:xs) = insertQuant (M.insert n (table x) mp) xs
    where n = fromIntegral $ qtNum x

insertHuff :: M.Map (Int,Int) HuffmanTree -> [HuffmanTable] -> M.Map (Int,Int) HuffmanTree
insertHuff mp [] = mp
insertHuff mp (x:xs) = insertHuff (M.insert (t,n) lvl mp) xs
    where t = fromIntegral $ htType x
          n = fromIntegral $ htNum x
          lvl = buildHuffmanTree $ zip (duplicateIdxs (map fromIntegral . htNumSymbols $ x)) 
                                       (map fromIntegral . htSymbols $ x)

updateJPEG :: Segment -> JPEG -> JPEG
updateJPEG seg state = case seg of
    Seg1 x -> state { fileHeader = x }
    Seg2 x -> state { infoHeader = x }
    Seg3 x -> let cur = quantTables state
                  nxt = qtTables x
              in state { quantTables = insertQuant cur nxt }
    Seg4 x -> let cur = huffmanTrees state
                  nxt = htTables x
              in state { huffmanTrees = insertHuff cur nxt }
    Seg5 x -> state { scanInfo = x }
    Seg6 x -> state { imageData = x }
    

-- apparently haskell doesnt like 2 + fromIntegral ...
getLength :: Segment -> Int64
getLength xx = case xx of
    Seg1 x -> 20
    Seg2 x -> (+2) . fromIntegral $ sofLength x
    Seg3 x -> (+2) . fromIntegral $ qtLength x
    Seg4 x -> (+2) . fromIntegral $ htLength x
    Seg5 x -> (+2) . fromIntegral $ sosLength x
    _      -> error "Undefined segment length"

readSegments :: BL.ByteString -> [Segment]
readSegments buf
  | marker == 0xFFD8 = let x = Seg1 $ decode buf in x : readSegments (BL.drop (getLength x) buf)
  | marker == 0xFFC0 = let x = Seg2 $ decode buf in x : readSegments (BL.drop (getLength x) buf)
  | marker == 0xFFDB = let x = Seg3 $ decode buf in x : readSegments (BL.drop (getLength x) buf)
  | marker == 0xFFC4 = let x = Seg4 $ decode buf in x : readSegments (BL.drop (getLength x) buf)
  | marker == 0xFFDA = let x = Seg5 $ decode buf in x : Seg6 (BL.drop (getLength x) buf) : []
    -- want to throw error but temporary workaround
  | otherwise = readSegments $ BL.dropWhile (/= 0xff) $ BL.tail buf 
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
          -- | trace ("rec2: " ++ show n ++ " " ++ show inf ++ " " ++ show run ++ " " ++ show sz ++ " " ++ show acc) False = undefined
          -- | n < 0 = error "Ya done fucked up"
          | run == 0 && sz == 0 = ((take n $ repeat 0) : acc, rst)
          | run == 15 && sz == 0 = rec rst ((take 16 $ (repeat 0)) : acc) (n - 16)
          | otherwise = rec cont ([roll numBits] : (take run $ repeat 0) : acc) (n - (run + 1))
          where (inf, rst) = huffmanTreeLookup hf ys
                (run, sz) = (fromIntegral $ inf `shift` (-4), fromIntegral $ inf .&. 0xF)
                (numBits, cont) = splitAt sz rst

roundUp8 :: Int -> Int
roundUp8 x = 1 + (x - 1) `div` 8

decodeJPEG :: BL.ByteString -> [([[Int]],[[Int]],[[Int]])]
decodeJPEG buf = accum (toBits . imageData $ jpeg) [] blockCnt [0,0,0]
    where 
        jpeg = readJPEG buf
        blockCnt = 
            let blocksX = roundUp8 . fromIntegral . sofWidth . infoHeader $ jpeg
                blocksY = roundUp8 . fromIntegral . sofHeight . infoHeader $ jpeg
            in blocksX * blocksY :: Int

        accum :: [Word8] -> [([[Int]],[[Int]],[[Int]])] -> Int -> [Int]
                  -> [([[Int]],[[Int]],[[Int]])]
        accum buf acc 0 _ = reverse acc
        accum buf acc n xs = accum rst (nxt:acc) (n-1) [a!!0!!0,b!!0!!0,c!!0!!0]
          where (hr, rst) = rec 0 [] buf xs
                nxt@(a,b,c) = toTuple hr

        rec :: Int -> [[[Int]]] -> [Word8] -> [Int]
               -> ([[[Int]]], [Word8])
        rec 3 acc stm _ = (reverse acc, stm)
        rec c acc stm (y:ys) = rec (c+1) (n:acc) rst ys
            where
                n = D.decode tab ((dc+y):ac)
                (_,x) = (sosComponents . scanInfo $ jpeg) !! c
                (acT,dcT) = (fromIntegral $ x .&. 0xF, fromIntegral $ x `shiftR` 4)
                (dc,tmp) = decodeDCValue (fromJust $ M.lookup (0,dcT) (huffmanTrees jpeg)) stm
                (ac,rst) = decodeACValues (fromJust $ M.lookup (1,acT) (huffmanTrees jpeg)) tmp
                Just tab = M.lookup (fromIntegral $ fcQuantNum ((sofComps . infoHeader $ jpeg) !! c)) (quantTables jpeg)

charIntensity :: Array Int Char
charIntensity = listArray (0,9) " .,:;xo%#@"

pixelToChar :: Word8 -> Char
pixelToChar x = charIntensity!(fromIntegral (255-x)*10`div`256)

tmpOut :: BL.ByteString -> [[Char]]
tmpOut buf = map (map (pixelToChar . fromIntegral)) lums
    where
        jpeg = readJPEG buf
        w = fromIntegral . sofWidth . infoHeader $ jpeg
        h = fromIntegral . sofHeight . infoHeader $ jpeg
        blocksX = roundUp8 w
        blocksY = roundUp8 h
        arr = groupN blocksX . take (blocksX * blocksY) . cycle . 
                map (\(a,_,_) -> a) $ decodeJPEG buf

        lums = map (take w) . take h . foldr (++) [] $ map merge arr
        merge :: [[[a]]] -> [[a]]
        merge xs = [foldr (++) [] $ map (!!i) xs | i <- [0..7]]

