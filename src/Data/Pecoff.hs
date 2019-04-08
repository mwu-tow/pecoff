-- | Parses a ByteString into a Pecoff record. Parsing of section data currently
-- left as a todo.
module Data.Pecoff where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import Control.Monad
import Control.Monad.IO.Class
import Data.Binary.Get
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word

import Data.Pecoff.Enums
import Data.Pecoff.Gettable
import Data.Pecoff.Header
import Data.Pecoff.Imports
import Data.Pecoff.OptionalHeader
import Data.Pecoff.RVA
import Data.Pecoff.Section

data Pecoff = Pecoff
            { coffHeader     :: CoffHeader
            , optionalHeader :: OptionalHeader
            , sections      :: [Section]
            } deriving (Show, Eq)

instance Addressable Pecoff where
  access = access . sections

-- | Reads PE file up to (including) PE header offset and returns it.
getPeOffset :: Get Int32
getPeOffset = do
    magic <- liftM (C.unpack . B.pack) $ sequence [getWord8, getWord8]
    if magic == "MZ" then do
        skip 0x3a
        get
    else
        fail "Invalid magic number in MSDOS header."
               
getPecoff :: B.ByteString -> Get Pecoff
getPecoff bs = do
    magic <- liftM (C.unpack . B.pack) $ sequence [getWord8, getWord8, getWord8, getWord8]
    unless (magic == "PE\0\0") $
        fail "Invalid magic number in PE header."

    coffHeader <- get
    optionalHeader <- get
    let sCount = fromIntegral $ sectionCount coffHeader
    sections <- sequence $ replicate sCount $ getSection bs

    pure $ Pecoff
        { coffHeader     = coffHeader
        , optionalHeader = optionalHeader
        , sections       = sections
        }
     
-- | Load and parse PE/COFF file into a Pecoff record.
readPecoff :: MonadIO m => FilePath -> m Pecoff
readPecoff path = parsePecoff <$> liftIO (B.readFile path)

-- | Parse the ByteString of a PE/COFF file into a Pecoff record.
parsePecoff :: B.ByteString -> Pecoff
parsePecoff bs =
    let coffOffset = runGet getPeOffset $ L.fromStrict bs
        coffObject = B.drop (fromIntegral $ coffOffset) bs
    in runGet (getPecoff bs) $ L.fromChunks [coffObject]

-- | Get pretty description of imports for parsed binary.
imports :: Pecoff -> [Import]
imports Pecoff{..} =
    let idts = concat $ maybeToList $ getAt' sections get <$> importTable optionalHeader
    in resolveImport (format optionalHeader) sections <$> idts

-- | Returns filenames of imported dynamic libraries for binary under given path.
dependenciesOfBinary 
    :: MonadIO m
    => FilePath  -- ^ Path to the PE/COFF binary to be checked.
    -> m [FilePath]
dependenciesOfBinary path = fmap libraryName <$> (imports <$> readPecoff path)