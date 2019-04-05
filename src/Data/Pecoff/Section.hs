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

-- See https://docs.microsoft.com/en-us/windows/desktop/debug/pe-format#section-table-section-headers
-- See https://docs.microsoft.com/en-us/windows/desktop/api/winnt/ns-winnt-_image_section_header

data Section = Section
    { name            :: String  -- ^ Name of section, up to eight UTF-8 characters.
    , virtualSize     :: Int32  -- ^ The total size of the section when loaded into memory. If this value is greater than 'rawDataSize', the section is zero-padded. This field is valid only for executable images and should be set to zero for object files. 
    , virtualAddress  :: Int32  -- ^ For executable images, the address of the first byte of the section relative to the image base when the section is loaded into memory. For object files, this field is the address of the first byte before relocation is applied; for simplicity, compilers should set this to zero. Otherwise, it is an arbitrary value that is subtracted from offsets during relocation. 
    , rawDataSize     :: Int32 -- ^ The size of the initialized data on disk, in bytes. This value must be a multiple of the 'Data.Pecoff.OptionalHeader.fileAlignment' member of the 'Data.Pecoff.OptionalHeader.OptionalHeader'. If this value is less than the 'virtualSize', the remainder of the section is filled with zeroes. If the section contains only uninitialized data, the member is zero.
    , rawDataPointer  :: Int32 -- ^ A file pointer to the first page within the COFF file. This value must be a multiple of the 'Data.Pecoff.OptionalHeader.fileAlignment' member of the 'Data.Pecoff.OptionalHeader.OptionalHeader'. If a section contains only uninitialized data, set this member is zero.
    , characteristics :: [SectionCharacteristics] -- ^ The flags that describe the characteristics of the section. For more information, see Section 'SectionCharacteristics'.
    -- , relocations     :: B.ByteString          -- ^ Raw data for relocations.
    -- , linenumbers     :: B.ByteString          -- ^ Raw data for linenumbers.
    , rawData         :: ByteString               -- ^ Raw data for section.
    } deriving (Eq)

-- | custom instance to avoid dropping raw section data
instance Show Section where
    show Section{..} = printf  "Section { name = %s, virtualSize = %d, rawDataSize = %d, rawDataPointer = %d, characteristics = %s"
                    name virtualSize rawDataSize rawDataPointer (show characteristics)

getSectionHeader ::  C.ByteString -> Get Section
getSectionHeader bs = do
    full_name            <- getByteString 8
    name                 <- return $ runGet get $ L.fromChunks[full_name]
    virtualSize          <- get
    virtualAddress       <- get
    sizeOfRawData        <- get
    pointerToRawData     <- get
    pointerToRelocations <- get :: Get Word32
    pointerToLinenumbers <- get :: Get Word32
    numberOfRelocations  <- get :: Get Word16
    numberOfLinenumbers  <- get :: Get Word16
    characteristics      <- get
    return $ Section
        { name            = name
        , virtualSize     = virtualSize
        , virtualAddress  = virtualAddress
        , rawDataSize     = sizeOfRawData
        , rawDataPointer  = pointerToRawData
        , characteristics = characteristics
        --  , relocations     = B.take (10 * numberOfRelocations) $ B.drop pointerToRelocations bs
        --  , linenumbers     = B.take (6 * numberOfLinenumbers) $ B.drop pointerToLinenumbers bs
        , rawData         = B.take (fromIntegral sizeOfRawData) $ B.drop (fromIntegral pointerToRawData) bs
        }

instance Addressable [Section] where
    access sections rva = B.drop offset $ rawData section where
        -- (section, offset) = case trace ("accessing rva " <> show rva) (rvaToSectionOffset rva sections) of
        (section, offset) = case rvaToSectionOffset rva sections of
            Just bs -> bs
            Nothing -> error $ "failed to find section containing RVA=" <> show rva

-- | Does given section contains data for given RVA
sectionContains :: RelativeVirtualAddress -> Section -> Bool
sectionContains (RelativeVirtualAddress rva) s = rva >= begin && rva < end where
    begin  = virtualAddress s
    end    = virtualSize    s + begin
            
-- | Find section that contains given RVA
lookupSection :: [Section] -> RelativeVirtualAddress -> Maybe Section
-- lookupSection allSections rva = trace ("address " <> show rva <> " found in " <> show (name <$> relevantSection)) relevantSection where
lookupSection allSections rva = relevantSection where
    isRelevant = sectionContains rva
    relevantSection = find isRelevant allSections

rvaToSectionOffset :: RelativeVirtualAddress -> [Section] -> Maybe (Section, Int)
rvaToSectionOffset rva@(RelativeVirtualAddress rvaInt) sections = do
    section   <- lookupSection sections rva
    let sectionVA = virtualAddress section
    let offset    = fromIntegral $ rvaInt - sectionVA
    pure $ (section, offset)        

rvaToFileOffset :: RelativeVirtualAddress -> [Section] -> Maybe Int
rvaToFileOffset rva sections = do
    (section, offset) <- rvaToSectionOffset rva sections -- FIXME
    pure $ offset + fromIntegral (rawDataPointer section)