data FileHeader = FileHeader {
        bfType :: Word16,
        bfSize :: Word32,
        bfReserved1 :: Word16,
        bfReserved2 :: Word16,
        bfOffBits :: Word32}

sizeFileHeader :: Int
sizeFileHeader = 14

data InfoHeader = InfoHeader {
        biSize :: Word32,
        biWidth :: Word32,
        biHeight :: Word32,
        biPlanes :: Word16,
        biBitCount :: Word16,
        biCompression :: Word32,
        biSizeImage :: Word32,
        biXPelsPerMeter :: Word32,
        biYPelsPerMeter :: Word32,
        biClrUsed :: Word32,
        biClrImportant :: Word32}

sizeInfoHeader :: Int
sizeInfoHeader = 40

type Pixel = (Word8, Word8, Word8)
data ColorTable = ColorTable {pixels :: [Pixel]}

decodeCT :: BL.ByteString -> ColorTable
encodeCT :: ColorTable -> BL.ByteString

data BMP = BMP {
         bmpFileHeader :: FileHeader,
         bmpInfoHeader :: InfoHeader,
         bmpColorTable :: Maybe ColorTable,
         imageData :: BL.ByteString}

readBMP :: BL.ByteString -> BMP

