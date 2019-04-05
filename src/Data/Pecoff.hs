-- | Parses a ByteString into a Pecoff record. Parsing of section data currently
-- left as a todo.
module Data.Pecoff where
                  -- ( Pecoff(..)
                  --  , Section(..)
                  --  , IMAGE_SUBSYSTEM(..)
                  --  , IMAGE_SCN_CHARACTERISTICS(..)
                  --  , IMAGE_DLL_CHARACTERISTICS(..)
                  --  , IMAGE_FILE_CHARACTERISTICS(..)
                  --  , IMAGE_FILE_MACHINE(..)
                  --  , OptionalHeader(..)
                  --  , CoffHeader(..)
                  --  , DataDirectory(..)
                  --  , parsePecoff
                  --  ) where


import Data.Pecoff.Enums
import Data.Pecoff.Header
import Data.Pecoff.OptionalHeader
import Data.Pecoff.Imports
import Data.Pecoff.Gettable
import Data.Pecoff.RVA
import Data.Pecoff.Section

import Data.Bits
import Data.Char
import Data.List
import Data.Int
import Data.Word
import Data.Binary.Get
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

data Pecoff = Pecoff
            { coffHeader     :: CoffHeader
            , optionalHeader :: OptionalHeader
            , pSections      :: [Section]  -- ^ Sections contained in this PE/COFF object.
            , importTables   :: [Import]
            } deriving (Show, Eq)

instance Addressable Pecoff where
  access = access . pSections

-- | Reads PE file up to (including) PE header offset and returns it.
getPeOffset :: Get Int32
getPeOffset = do
  magic <- liftM (C.unpack . B.pack) $ sequence [getWord8, getWord8]
  if magic == "MZ" then do
     skip 0x3a
     coffOffset <- liftM fromIntegral getWord32le
     return coffOffset
   else
     fail "Invalid magic number in MSDOS header."
               
getPecoff :: C.ByteString -> Get Pecoff
getPecoff bs = do
  magic <- liftM (C.unpack . B.pack) $ sequence [getWord8, getWord8, getWord8, getWord8]
  if magic /= "PE\0\0" then
      fail "Invalid magic number in PE header."
  else do
    coffHeader <- get
    optionalHeader <- get
    sections <- sequence $ replicate (fromIntegral $ sectionCount coffHeader) (getSectionHeader bs)

    let idts = case importTable optionalHeader of
          Just dataDir -> getAt' sections get dataDir
          Nothing      -> []
            
    pure $ Pecoff
      { coffHeader     = coffHeader
      , optionalHeader = optionalHeader
      , pSections      = sections
      , importTables   = resolveImport (format optionalHeader) sections <$> idts
      }
     
-- | Parse the ByteString of a PE/COFF file into a Pecoff record.
parsePecoff :: B.ByteString -> Pecoff
parsePecoff bs =
  let coffOffset = runGet getPeOffset $ L.fromChunks [bs]
      coffObject = B.drop (fromIntegral $ coffOffset) bs
  in runGet (getPecoff bs) $ L.fromChunks [coffObject]
  