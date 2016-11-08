import Util
import BMP
import qualified Data.ByteString.Lazy as BL
import Data.Array
import Data.Binary (Word8)


charIntensity :: Array Int Char
charIntensity = listArray (0,9) " .,:;ox%#@"

pixelToChar :: Word8 -> Char
pixelToChar x = charIntensity!(fromIntegral (255-x)*10`div`256)

bmpToASCII :: BMP -> [[Char]]
bmpToASCII bmp = groupN w . reverse . map pixelToChar .  BL.unpack . imageData $ bmp
    where (w,_) = dimensionsBMP bmp

convertBMPFileToASCII :: FilePath -> FilePath -> IO ()
convertBMPFileToASCII inFile outFile = do
    file <- BL.readFile inFile
    writeFile outFile $ unlines . bmpToASCII . readBMP $ file
