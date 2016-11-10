module ASCII (
    bmpToASCII,
    convertBMPFileToASCII
    ) where

import Util
import BMP
import PNG
import qualified Data.ByteString.Lazy as BL
import Data.Array
import Data.Binary (Word8)


charIntensity :: Array Int Char
charIntensity = listArray (0,9) " .,:;xo%#@"

pixelToChar :: Word8 -> Char
pixelToChar x = charIntensity!(fromIntegral (255-x)*10`div`256)

-- NOTE: currently assumes that colortable is filled 0-255
bmpToASCII :: BMP -> [[Char]]
bmpToASCII bmp = groupN w . reverse . map pixelToChar .  BL.unpack . bmpImageData $ bmp
    where (w,_) = dimensionsBMP bmp

convertBMPFileToASCII :: FilePath -> FilePath -> IO ()
convertBMPFileToASCII inFile outFile = do
    file <- BL.readFile inFile
    writeFile outFile $ unlines . bmpToASCII . readBMP $ file

pngToASCII :: PNG -> [[Char]]
pngToASCII png = let (f,dat) = getTmp png in
                    map (everyN 3) . map (map pixelToChar) $ reconstruct f dat

convertPNGFileToASCII :: FilePath -> FilePath -> IO ()
convertPNGFileToASCII inFile outFile = do
    file <- BL.readFile inFile
    writeFile outFile $ unlines . pngToASCII . readPNG $ file
