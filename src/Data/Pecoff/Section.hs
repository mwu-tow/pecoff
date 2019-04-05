module Data.Pecoff.Section where 

import Data.Pecoff.Enums.SectionCharacteristics
import Data.Pecoff.Gettable
import Data.Pecoff.RVA

import Data.Maybe
import Data.Word
import Data.Int
import Data.List (find)
import Data.ByteString (ByteString)
import Data.Binary.Get
import Debug.Trace
import Text.Printf

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Text.Printf

-- See https://docs.microsoft.com/en-us/windows/desktop/debug/pe-format#section-table-section-headers
-- See https://docs.microsoft.com/en-us/windows/desktop/api/winnt/ns-winnt-_image_section_header


data Section = Section
    { sHeader :: SectionHeader
    -- , relocations     :: B.ByteString          -- ^ Raw data for relocations.
    -- , linenumbers     :: B.ByteString          -- ^ Raw data for linenumbers.
    , sData :: SectionData -- ^ Raw data for section.
    } 
    deriving (Eq, Show)

data SectionHeader = SectionHeader
    { name            :: String  -- ^ Name of section, up to eight UTF-8 characters.
    , virtualSize     :: Int32  -- ^ The total size of the section when loaded into memory. If this value is greater than 'rawDataSize', the section is zero-padded. This field is valid only for executable images and should be set to zero for object files. 
    , virtualAddress  :: Int32  -- ^ For executable images, the address of the first byte of the section relative to the image base when the section is loaded into memory. For object files, this field is the address of the first byte before relocation is applied; for simplicity, compilers should set this to zero. Otherwise, it is an arbitrary value that is subtracted from offsets during relocation. 
    , rawDataSize     :: Int32 -- ^ The size of the initialized data on disk, in bytes. This value must be a multiple of the 'Data.Pecoff.OptionalHeader.fileAlignment' member of the 'Data.Pecoff.OptionalHeader.OptionalHeader'. If this value is less than the 'virtualSize', the remainder of the section is filled with zeroes. If the section contains only uninitialized data, the member is zero.
    , rawDataPointer  :: Int32 -- ^ A file pointer to the first page within the COFF file. This value must be a multiple of the 'Data.Pecoff.OptionalHeader.fileAlignment' member of the 'Data.Pecoff.OptionalHeader.OptionalHeader'. If a section contains only uninitialized data, set this member is zero.
    , relocationsPointer :: Int32 -- ^ The file pointer to the beginning of relocation entries for the section. This is set to zero for executable images or if there are no relocations. 
    , linenumbersPointer :: Int32 -- ^ The file pointer to the beginning of line-number entries for the section. This is set to zero if there are no COFF line numbers. This value should be zero for an image because COFF debugging information is deprecated. 
    , relocationsCount :: Int16 -- ^ The number of relocation entries for the section. This is set to zero for executable images. 
    , linenumberCount :: Int16 -- ^ The number of line-number entries for the section. This value should be zero for an image because COFF debugging information is deprecated. 
    , characteristics :: [SectionCharacteristics] -- ^ The flags that describe the characteristics of the section. For more information, see SectionHeader 'SectionCharacteristics'.
    } deriving (Eq, Show)

-- | Custom type wrapping ByteString, so we can add custom show that doesn't dump all the raw data
newtype SectionData = SectionData { rawData :: ByteString } deriving (Eq)

-- | Just logs binary size, not its contents
instance Show SectionData where
    show (SectionData bs) = printf "[binary, %d bytes]" $ B.length bs

instance Gettable SectionHeader where
    get = do
        name               <- getBS <$> getByteString 8
        virtualSize        <- get
        virtualAddress     <- get
        sizeOfRawData      <- get
        pointerToRawData   <- get
        relocationsPointer <- get
        linenumbersPointer <- get
        relocationsCount   <- get
        linenumberCount    <- get
        characteristics    <- get
        pure $ SectionHeader
            { name            = name
            , virtualSize     = virtualSize
            , virtualAddress  = virtualAddress
            , rawDataSize     = sizeOfRawData
            , rawDataPointer  = pointerToRawData
            , characteristics = characteristics
            , relocationsPointer = relocationsPointer
            , linenumbersPointer = linenumbersPointer
            , relocationsCount = relocationsCount
            , linenumberCount = linenumberCount
            }


substring 
    :: (Integral i1, Integral i2) 
    => ByteString
    -> i1 -- ^ Offset
    -> i2 -- ^ Length
    -> ByteString
substring bs offset length = 
    B.take (fromIntegral length) 
    $ B.drop (fromIntegral offset) bs


obtainSectionData :: ByteString -> SectionHeader -> SectionData
obtainSectionData bs SectionHeader{..} = SectionData $ substring bs rawDataPointer rawDataSize

completeSection :: ByteString -> SectionHeader -> Section
completeSection bs header = Section
    { sHeader = header
    , sData = obtainSectionData bs header
    }

getSection ::  C.ByteString -> Get Section
getSection bs = completeSection bs <$> get

instance Addressable [Section] where
    access sections rva = B.drop offset $ rawData $ sData section where
        -- (section, offset) = case trace ("accessing rva " <> show rva) (rvaToSectionOffset rva sections) of
        (section, offset) = case rvaToSectionOffset rva sections of
            Just bs -> bs
            Nothing -> error $ "failed to find section containing RVA=" <> show rva

-- | Does given section contains data for given RVA
sectionContains :: RelativeVirtualAddress -> SectionHeader -> Bool
sectionContains (RelativeVirtualAddress rva) s = rva >= begin && rva < end where
    begin  = virtualAddress s
    end    = virtualSize    s + begin
            
-- | Find section that contains given RVA
lookupSection :: [Section] -> RelativeVirtualAddress -> Maybe Section
-- lookupSection allSections rva = trace ("address " <> show rva <> " found in " <> show (name <$> relevantSection)) relevantSection where
lookupSection allSections rva = relevantSection where
    isRelevant = sectionContains rva . sHeader
    relevantSection = find isRelevant allSections

rvaToSectionOffset :: RelativeVirtualAddress -> [Section] -> Maybe (Section, Int)
rvaToSectionOffset rva@(RelativeVirtualAddress rvaInt) sections = do
    section   <- lookupSection sections rva
    let sectionVA = virtualAddress $ sHeader section
    let offset    = fromIntegral $ rvaInt - sectionVA
    pure $ (section, offset)        

rvaToFileOffset :: RelativeVirtualAddress -> [Section] -> Maybe Int
rvaToFileOffset rva sections = do
    (section, offset) <- rvaToSectionOffset rva sections
    pure $ offset + fromIntegral (rawDataPointer $ sHeader section)