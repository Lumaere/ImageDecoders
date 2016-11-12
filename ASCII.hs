module ASCII (
    bmpToASCII,
    convertBMPFileToASCII,
    pngToASCII,
    convertPNGFileToASCII,
    ) where

import Util
import BMP
import PNG
import JPEG (decodeJPEG)
import ColorFormats
import qualified Data.ByteString.Lazy as BL
import Data.Array
import Data.Binary (Word8)


charIntensity :: Array Int Char
charIntensity = listArray (0,9) " .,:;xo%#@"

pixelToChar :: Int -> Char
pixelToChar x = charIntensity!(fromIntegral (255-y)*10`div`256)
    where y = if x > 255 then 255 else if x < 0 then 0 else x

-- NOTE: currently assumes that colortable is filled 0-255
bmpToASCII :: BMP -> [[Char]]
bmpToASCII bmp = groupN w . reverse . map (pixelToChar . fromIntegral) .  BL.unpack . bmpImageData $ bmp
    where (w,_) = dimensionsBMP bmp

convertBMPFileToASCII :: FilePath -> FilePath -> IO ()
convertBMPFileToASCII inFile outFile = do
    file <- BL.readFile inFile
    writeFile outFile $ unlines . bmpToASCII . readBMP $ file

pngToASCII :: PNG -> [[Char]]
pngToASCII png = let s = case colourType . pngInfoHeader $ png of
                            2 -> 3
                            6 -> 4
                            _ -> error "Unhandled image type"
                     w = fromIntegral . width . pngInfoHeader $ png
                 in groupN w . map (pixelToChar . fromIntegral) . everyN s . BL.unpack . pngImageData $ png

convertPNGFileToASCII :: FilePath -> FilePath -> IO ()
convertPNGFileToASCII inFile outFile = do
    file <- BL.readFile inFile
    writeFile outFile $ unlines . pngToASCII . readPNG $ file

pixelsChromaToASCII :: [[(Int,Int,Int)]] -> [[Char]]
pixelsChromaToASCII = map (map (pixelToChar . rgbToGray . yCbCrToRGB))

convertJPEGFileToASCII :: FilePath -> FilePath -> IO ()
convertJPEGFileToASCII inFile outFile = do
    file <- BL.readFile inFile
    writeFile outFile $ unlines . pixelsChromaToASCII . decodeJPEG $ file


