module Data.Pecoff.Pecoff where

import Control.Monad
import Data.Binary.Get
import Data.Int
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

import Data.Pecoff.Gettable
import Data.Pecoff.Header
import Data.Pecoff.OptionalHeader
import Data.Pecoff.Section
import Data.Pecoff.RVA

-- | Parsed PE/COFF file.
data Pecoff = Pecoff
            { coffHeader     :: CoffHeader
            , optionalHeader :: OptionalHeader
            , sections      :: [Section]
            } deriving (Show, Eq)

-- | PE/COFF file contains 'Data.Pecoff.sections' with raw, 'Addresable' data.
instance Addressable Pecoff where
    access = access . sections
    
-- | Cnverts String to a list of 'Word8'-bytes. String must be ascii-only.
toAsciiBytes :: String -> [Word8]
toAsciiBytes = B.unpack . C.pack

-- | Reads PE file up to (including) PE header offset and returns it.
getPeOffset :: Get Int32
getPeOffset = do
    magic <- sequence [getWord8, getWord8]
    if magic == toAsciiBytes "MZ"
        then do
            skip 0x3a
            get
        else fail "Invalid magic number in MSDOS header."

-- | Reads PE/COFF from 'B.ByteString' input starting at PE Header (i.e. with
-- DOS section already dropped).
getPecoff :: B.ByteString -> Get Pecoff
getPecoff bs = do
    magic <- sequence [getWord8, getWord8, getWord8, getWord8]
    unless (magic == toAsciiBytes "PE\0\0") $
        fail "Invalid magic number in PE header."

    coffHeader <- get
    optionalHeader <- get
    let sCount = fromIntegral $ sectionCount coffHeader
    sections <- sequence $ replicate sCount $ getSection bs
    pure $ Pecoff{..}
     
-- | Parse the ByteString of a PE/COFF file into a 'Pecoff' record.
parsePecoff :: B.ByteString -> Pecoff
parsePecoff bs =
    let coffOffset = runGet' getPeOffset bs
        coffObject = B.drop (fromIntegral $ coffOffset) bs
    in runGet (getPecoff bs) $ L.fromChunks [coffObject]
    