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
import Data.Pecoff.Headers
import Data.Pecoff.Gettable

import Data.Bits
import Data.Char
import Data.List
import Data.Int
import Data.Word
import Data.Binary.Get
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
           
type AdressGetter = Get Word64

addressGetter :: Bool -> AdressGetter
addressGetter True = getWord64le
addressGetter False = liftM fromIntegral getWord32le

checkIfPE32Plus :: Word16 -> Bool
checkIfPE32Plus = \case
  0x020b -> True
  0x010b -> False
  _      -> error "Invalid magic number for image file optional header."                         
                   
data Pecoff = Pecoff
            { coffHeader     :: CoffHeader
            , optionalHeader :: OptionalHeader
            , pSections      :: [Section]  -- ^ Sections contained in this PE/COFF object.
            , importTables   :: [Import]
            } deriving (Show, Eq)

-- | Assumes 4-byte second count from epoch start
getTimestamp :: Get UTCTime
getTimestamp =  (posixSecondsToUTCTime . fromInteger . fromIntegral) <$> getWord32le

-- | Standard COFF File Header
data CoffHeader = CoffHeader
  { machine             :: Machine -- ^ Target machine type
  , sectionCount        :: Int16   -- ^ Number of sections.
  , timestamp           :: UTCTime -- ^ The date and time when the image was created by the linker.
  , symbolTableOffset   :: Int32 -- ^ The offset of the symbol table, in bytes, or zero if no COFF symbol table exists.
  , symbolCount         :: Int32 -- ^ The number of symbols in the symbol table.
  , optionalHeaderSize  :: Int32 -- ^ The size of the optional header, in bytes. This value should be 0 for object files.
  , fileCharacteristics :: [Characteristics]
  } 
  deriving (Show, Eq)

type RelativeVirtualAddress = Int32

getRva :: Get RelativeVirtualAddress
getRva = fromIntegral <$> getWord32le

data DataDirectory = DataDirectory
  { address :: RelativeVirtualAddress
  , size    :: Int64
  }
  deriving (Show, Eq)

getDataDirectory :: Get DataDirectory
getDataDirectory = do
  rva <- getRva
  size <- getWord32le
  pure $ DataDirectory
    { address = rva
    , size = fromIntegral size 
    }
-- | Optional header with information for loader. Despite its name it is required for all image files. It is optional e.g. for object files.
data OptionalHeader = OptionalHeader 
  { isPE32Plus        :: Bool
  , entryPointAddress :: Int32            -- ^ Entry point address, relative to the image base.
  , imageBase         :: Int64            -- ^ Preferred load base address of image.
  , subsystem         :: Subsystem  -- ^ Subsystem required to run this image. 
  , dllCharactertics  :: [DllCharacteristics]
  , importTableRVA    :: Maybe DataDirectory
  }
  deriving (Show, Eq)

getImageFileHeader :: Get CoffHeader
getImageFileHeader = do
  machine <- get
  numsect <- getWord16le
  tmstamp <- getTimestamp
  symtoff <- getWord32le
  symtcnt <- getWord32le
  szopthd <- getWord16le
  characteristics <- get
  pure $ CoffHeader
    { machine             = machine
    , sectionCount        = fromIntegral numsect
    , timestamp           = tmstamp
    , symbolTableOffset   = fromIntegral symtoff
    , symbolCount         = fromIntegral symtcnt
    , optionalHeaderSize  = fromIntegral szopthd
    , fileCharacteristics = characteristics
    }
  -- return (machine, numsect, attribs)

getOptionalHeader :: Get OptionalHeader
getOptionalHeader = do
  magic                   <- getWord16le
  let isPE32Plus          = checkIfPE32Plus magic
  let getAddress          = addressGetter isPE32Plus
  majorLinkerVersion      <- getWord8
  minorLinkerVersion      <- getWord8
  sizeOfCode              <- getWord32le
  sizeOfInitializedData   <- getWord32le
  sizeOfUninitializedData <- getWord32le
  addressOfEntryPoint     <- liftM fromIntegral getWord32le
  baseOfCode              <- getWord32le
  baseOfData              <- if isPE32Plus then return 0 else getWord32le
  imageBase               <- getAddress
  sectionAlignment        <- getWord32le
  fileAlignment           <- getWord32le
  majorOSVersion          <- getWord16le
  minorOSVersion          <- getWord16le
  majorImageVersion       <- getWord16le
  minorImageVersion       <- getWord16le
  majorSubSystemVersion   <- getWord16le
  minorSubSystemVersion   <- getWord16le
  win32VersionValue       <- getWord32le
  sizeOfImage             <- getWord32le
  sizeOfHeaders           <- getWord32le
  checksum                <- getWord32le
  subsystem               <- get
  dllCharacteristics      <- get
  sizeOfStackReserve      <- getAddress
  sizeOfStackCommit       <- getAddress
  sizeOfHeapReserve       <- getAddress
  sizeOfHeapCommit        <- getAddress
  loaderFlags             <- getWord32le
  numberOfRvaAndSizes     <- liftM fromIntegral $ getWord32le
  imageDataDirectory      <- sequence $ replicate numberOfRvaAndSizes getDataDirectory
  skip (8 * (16 - numberOfRvaAndSizes))                           
       
  let exportTable           = if length imageDataDirectory >  0 then Just (imageDataDirectory !!  0) else Nothing
      importTable           = if length imageDataDirectory >  1 then Just (imageDataDirectory !!  1) else Nothing
      resourceTable         = if length imageDataDirectory >  2 then Just (imageDataDirectory !!  2) else Nothing
      exceptionTable        = if length imageDataDirectory >  3 then Just (imageDataDirectory !!  3) else Nothing
      certificateTable      = if length imageDataDirectory >  4 then Just (imageDataDirectory !!  4) else Nothing
      baseRelocationTable   = if length imageDataDirectory >  5 then Just (imageDataDirectory !!  5) else Nothing
      debug                 = if length imageDataDirectory >  6 then Just (imageDataDirectory !!  6) else Nothing
      architecture          = if length imageDataDirectory >  7 then Just (imageDataDirectory !!  7) else Nothing
      globalPtr             = if length imageDataDirectory >  8 then Just (imageDataDirectory !!  8) else Nothing
      tlsTable              = if length imageDataDirectory >  9 then Just (imageDataDirectory !!  9) else Nothing
      loadConfigTable       = if length imageDataDirectory > 10 then Just (imageDataDirectory !! 10) else Nothing
      boundImport           = if length imageDataDirectory > 11 then Just (imageDataDirectory !! 11) else Nothing
      iat                   = if length imageDataDirectory > 12 then Just (imageDataDirectory !! 12) else Nothing
      delayImportDescriptor = if length imageDataDirectory > 13 then Just (imageDataDirectory !! 13) else Nothing
      clrRuntimeHeader      = if length imageDataDirectory > 14 then Just (imageDataDirectory !! 14) else Nothing
  pure $ OptionalHeader
    { isPE32Plus        = isPE32Plus
    , entryPointAddress = addressOfEntryPoint
    , imageBase         = fromIntegral imageBase
    , subsystem         = subsystem
    , dllCharactertics  = dllCharacteristics
    , importTableRVA    = importTable
    }

getCharUTF8 :: Get Word32
getCharUTF8 = do
  let getCharUTF82 b1 = do
        b2 <- liftM fromIntegral getWord8 :: Get Word32
        if b2 .&. 0xc0 == 0x80 then
          return $ ((b1 .&. 0x1f) `shiftL` 6) .|. (b2 .&. 0x3f)
         else
          fail "Invalid second byte in UTf8 string."
      getCharUTF83 b1 = do
        b2 <- liftM fromIntegral getWord8 :: Get Word32
        b3 <- liftM fromIntegral getWord8 :: Get Word32
        if b2 .&. 0xc0 == 0x80 && b3 .&. 0xc0 == 0x80 then
          return $ ((b1 .&. 0x0f) `shiftL` 12) .|. ((b2 .&. 0x3f) `shiftL` 6) .|. (b3 .&. 0x3f)
         else
          fail "Invalid second or third byte in UTf8 string."
      getCharUTF84 b1 = do
        b2 <- liftM fromIntegral getWord8 :: Get Word32
        b3 <- liftM fromIntegral getWord8 :: Get Word32
        b4 <- liftM fromIntegral getWord8 :: Get Word32
        if b2 .&. 0xc0 == 0x80 && b3 .&. 0xc0 == 0x80 && b4 .&. 0xc0 == 0x80 then
          return $ ((b1 .&. 0x07) `shiftL` 18) .|. ((b2 .&. 0x3f) `shiftL` 12) .|. ((b3 .&. 0x3f) `shiftL` 6) .|. (b4 .&. 0x3f)
         else
          fail "Invalid second or third byte in UTf8 string."
  b1 <- liftM fromIntegral getWord8 :: Get Word32
  case b1 of
    n | n .&. 0x80 == 0x00 -> return $ fromIntegral n
    n | n .&. 0xe0 == 0xc0 -> getCharUTF82 n
    n | n .&. 0xf0 == 0xe0 -> getCharUTF83 n
    n | n .&. 0xf8 == 0xf0 -> getCharUTF84 n
    _                      -> fail "Invalid first byte in UTF8 string."

getUtf8String :: Get String
getUtf8String = liftM (map (chr . fromIntegral)) getUtf8String_
    where getUtf8String_ = do
            empty <- isEmpty
            if empty then
                return []
             else do
              char <- getCharUTF8
              if char == 0 then
                  return []
               else do
                rest <- getUtf8String_
                return (char : rest)

data Section = Section
  { name            :: String                      -- ^ Name of section.
  , virtualSize     :: Int32                       -- ^ Virtual memory size.
  , virtualAddress  :: Int32                       -- ^ Virtual memory address.
  , rawDataSize     :: Int32
  , rawDataPointer  :: Int32
  , characteristics :: [SectionCharacteristics] -- ^ Flags.
  -- , relocations     :: B.ByteString                -- ^ Raw data for relocations.
  -- , linenumbers     :: B.ByteString                -- ^ Raw data for linenumbers.
  , rawData         :: B.ByteString                -- ^ Raw data for section.
  } deriving (Show, Eq)

getSectionHeader ::  C.ByteString -> Get Section
getSectionHeader bs = do
  full_name            <- getByteString 8
  name                 <- return $ runGet getUtf8String $ L.fromChunks[full_name]
  virtualSize          <- liftM fromIntegral getWord32le
  virtualAddress       <- liftM fromIntegral getWord32le
  sizeOfRawData        <- liftM fromIntegral getWord32le
  pointerToRawData     <- liftM fromIntegral getWord32le
  pointerToRelocations <- liftM fromIntegral getWord32le
  pointerToLinenumbers <- liftM fromIntegral getWord32le
  numberOfRelocations  <- liftM fromIntegral getWord16le
  numberOfLinenumbers  <- liftM fromIntegral getWord16le
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
    coffHeader <- getImageFileHeader
    optionalHeader <- getOptionalHeader
    sections <- sequence $ replicate (fromIntegral $ sectionCount coffHeader) (getSectionHeader bs)

    let idts = case importTableRVA optionalHeader of
          Just dataDir -> getAt' sections getImportDirectoryTables dataDir
          Nothing      -> []
            
    pure $ Pecoff
      { coffHeader     = coffHeader
      , optionalHeader = optionalHeader
      , pSections      = sections
      , importTables   = resolveImport (isPE32Plus optionalHeader) sections <$> idts
      }
     
-- | Parse the ByteString of a PE/COFF file into a Pecoff record.
parsePecoff :: B.ByteString -> Pecoff
parsePecoff bs =
  let coffOffset = runGet getPeOffset $ L.fromChunks [bs]
      coffObject = B.drop (fromIntegral $ coffOffset) bs
  in runGet (getPecoff bs) $ L.fromChunks [coffObject]
  
data ImportDirectoryTable = ImportDirectoryTable
  { lookupTable        :: RelativeVirtualAddress
  , timestamp          :: UTCTime
  , firstForwarder     :: Int32
  , nameAddress        :: RelativeVirtualAddress
  , importAddressTable :: RelativeVirtualAddress
  }
  deriving (Show, Eq)

type ImportLookupTable = [ImportLookupEntry]

data ImportLookupEntry 
  = OrdinalImportLookupEntry Int16 
  | NameImportLookupEntry RelativeVirtualAddress
  deriving (Show, Eq)
 
data ImportHintNameEntry = ImportHintNameEntry
  { hint :: Int16
  , importName :: String
  }
  deriving (Show, Eq)

getImportHintNameEntry :: Get ImportHintNameEntry
getImportHintNameEntry = do
  hint <- getWord16le
  name <- getUtf8String
  pure $ ImportHintNameEntry 
    { hint = fromIntegral hint
    , importName = name 
    }


nullImportLookupEntry = NameImportLookupEntry 0

importLookupEntry:: (Integral bits, FiniteBits bits) => bits -> ImportLookupEntry
importLookupEntry val = 
  let maskBit = finiteBitSize val - 1
      unmasked = fromIntegral $ clearBit val maskBit
  in if testBit val maskBit
    then OrdinalImportLookupEntry $ fromIntegral $ unmasked
    else NameImportLookupEntry    $ fromIntegral $ unmasked

getImportLookupEntry 
  :: Bool  -- ^ Is PE32+ executable
  -> Get ImportLookupEntry
getImportLookupEntry True  = importLookupEntry <$> getWord64le
getImportLookupEntry False = importLookupEntry <$> getWord32le
  
getImportEntries 
  :: Bool  -- ^ Is PE32+ executable
  -> Get [ImportLookupEntry]
getImportEntries pe32type = do
  ie <- getImportLookupEntry pe32type
  if ie == nullImportLookupEntry
    then pure []
    else do
      (ie :) <$> getImportEntries pe32type

data Import = Import
  { libraryName :: String
  , entries     :: [ImportedFunction]
  }
  deriving (Show, Eq)

data ImportedFunction 
  = FunctionName String
  | FunctionOrdinal Int
  deriving (Show, Eq)

getImportDirectoryTable :: Get ImportDirectoryTable
getImportDirectoryTable = do
  lookupTable <- getRva
  timestamp <- getTimestamp
  firstForwarder <- fromIntegral <$> getWord32le
  nameAddress <- getRva
  importAddressTable <- getRva
  pure $ ImportDirectoryTable
    { lookupTable        = lookupTable
    , timestamp          = timestamp
    , firstForwarder     = firstForwarder
    , nameAddress        = nameAddress
    , importAddressTable = importAddressTable
    }

getImportDirectoryTables :: Get [ImportDirectoryTable]
getImportDirectoryTables = do
  idt <- getImportDirectoryTable
  case nameAddress idt of
    0 -> pure []
    _ -> do
      (idt :) <$> getImportDirectoryTables
     
-- getImportLookupTable :: ImportLookupTable

resolveLookupEntry :: [Section] -> ImportLookupEntry -> ImportedFunction
resolveLookupEntry _ (OrdinalImportLookupEntry ordinal) = FunctionOrdinal $ fromIntegral ordinal
resolveLookupEntry s (NameImportLookupEntry nameHintRva) = 
  let nameHintEntry = getAt s getImportHintNameEntry nameHintRva
  in FunctionName (importName nameHintEntry)

resolveImport :: Bool -> [Section] -> ImportDirectoryTable -> Import
resolveImport isPe32Plus s idt = 
  let libraryName = getAt s getUtf8String $ nameAddress idt
      entries = getAt s (getImportEntries isPe32Plus) $ importAddressTable idt

  in Import
    { libraryName = libraryName
    , entries = resolveLookupEntry s <$> entries
    }

getAt :: [Section] -> Get a -> RelativeVirtualAddress -> a
getAt s g rva = runGet g lbs where
    lbs = L.fromStrict bsAt
    bsAt = rvaToBS rva s

getAt' :: [Section] -> Get a -> DataDirectory -> a
getAt' s g d = runGet g lbs where
  lbs = L.fromStrict $ dataDirToBS d s

dataDirToBS :: DataDirectory -> [Section] -> B.ByteString
dataDirToBS datum sections = B.take length $ rvaToBS rva sections where
    length = fromIntegral $ size datum
    rva = address datum

sectionContains :: RelativeVirtualAddress -> Section -> Bool
sectionContains rva s = rva >= begin && rva < end where
    begin = virtualAddress s
    end   = virtualSize    s + begin

getSection :: [Section] -> RelativeVirtualAddress -> Section
getSection allSections rva = relevantSection where
    isRelevant = sectionContains rva
    relevantSection = case find isRelevant allSections of
        Just s -> s
        Nothing -> error $ "failed to find section containing RVA=" <> show rva

rvaToBS :: RelativeVirtualAddress -> [Section] -> B.ByteString
rvaToBS rva sections = B.drop offset $ rawData section where
    (section, offset) = rvaToSectionOffset rva sections
            
rvaToFileOffset :: RelativeVirtualAddress -> [Section] -> Int
rvaToFileOffset rva sections = offset + fromIntegral (rawDataPointer section) where
    (section, offset) = rvaToSectionOffset rva sections
    
rvaToSectionOffset :: RelativeVirtualAddress -> [Section] -> (Section, Int)
rvaToSectionOffset rva sections = (section, offset) where
    offset    = fromIntegral $ rva - sectionVA
    sectionVA = virtualAddress section
    section   = getSection sections rva