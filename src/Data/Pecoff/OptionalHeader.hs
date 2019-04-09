module Data.Pecoff.OptionalHeader where

import Data.Pecoff.Gettable
import Data.Pecoff.Enum
import Data.Pecoff.Enums.Format
import Data.Pecoff.Enums.Subsystem
import Data.Pecoff.Enums.DllCharacteristics
import Data.Pecoff.Gettable
import Data.Pecoff.RVA
import Data.Int
import Data.Word
import Data.Binary.Get
import Control.Monad
import Data.List

data DataDirectory = DataDirectory
  { address :: RelativeVirtualAddress
  , size    :: Int32
  }
  deriving (Show, Eq)

instance AddressSize DataDirectory where
    addressSize DataDirectory{..} = (address, fromIntegral size)

instance Gettable DataDirectory where
    get = DataDirectory <$> get <*> get

-- | Optional header with information for loader. Despite its name it is required for all image files. It is optional e.g. for object files.
data OptionalHeader = OptionalHeader 
  { format            :: PEFormat -- ^ 'PE32Plus' images allow for a 64-bit address space while limiting the image size to 2 gigabytes.
  , entryPointAddress :: Word32 -- ^ Entry point address, relative to the image base.
  , imageBase         :: Word64 -- ^ Preferred load base address of image.
  , subsystem         :: Subsystem  -- ^ Subsystem required to run this image. 
  , dllCharactertics  :: [DllCharacteristics]
  , sizeOfStackReserve:: Word64 -- ^ The size of the stack to reserve. Only 'sizeOfStackCommit' is committed; the rest is made available one page at a time until the reserve size is reached. 
  , sizeOfStackCommit :: Word64 -- ^ The size of the stack to commit.
  , sizeOfHeapReserve :: Word64 -- ^ The size of the local heap space to reserve. Only 'sizeOfHeapCommit' is committed; the rest is made available one page at a time until the reserve size is reached. 
  , sizeOfHeapCommit :: Word64 -- ^ The size of the local heap space to commit. 
  , dataDirectories :: [DataDirectory] -- ^ Set of data directories, each describing rva location and size
  }
  deriving (Show, Eq)

instance Gettable OptionalHeader where
    get = do
        format                   <- get
        let getAddress          = addressGetter format
        majorLinkerVersion      <- getWord8
        minorLinkerVersion      <- getWord8
        sizeOfCode              <- getWord32le
        sizeOfInitializedData   <- getWord32le
        sizeOfUninitializedData <- getWord32le
        addressOfEntryPoint     <- get
        baseOfCode              <- getWord32le
        baseOfData              <- if format == PE32Plus then return 0 else getWord32le
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
        when (loaderFlags /= 0) $
            fail $ "loaderFlags in the optional header is required to be zero, found: " <> show loaderFlags
        numberOfRvaAndSizes     <- fromIntegral <$> getWord32le
        imageDataDirectory      <- sequence $ replicate numberOfRvaAndSizes get
        skip (8 * (16 - numberOfRvaAndSizes))                           
        pure $ OptionalHeader
            { format         = format
            , entryPointAddress  = addressOfEntryPoint
            , imageBase          = imageBase
            , subsystem          = subsystem
            , dllCharactertics   = dllCharacteristics
            , sizeOfStackReserve = sizeOfStackReserve
            , sizeOfStackCommit  = sizeOfStackCommit 
            , sizeOfHeapReserve  = sizeOfHeapReserve  
            , sizeOfHeapCommit   = sizeOfHeapCommit  
            , dataDirectories    = imageDataDirectory
            }

-- | Gets element at a given index. Returns Nothing if the index is out of bounds.
safeAt :: Int -> [a] -> Maybe a
safeAt index list = if index >= 0 && index < length list
    then Just $ list !! index
    else Nothing

-- | Data Directory is null, if both its RVA and size are set to 0. Null data
-- directories are often used to denote that there is no given entry in the
-- executable.
isNullDataDir :: DataDirectory -> Bool
isNullDataDir DataDirectory{..} = address == nullRva && size == 0

-- | Obtains 'DataDirectory' by given index. Returns Nothing if index is out of
-- bounds or if the requested Data Directory is null.
dataDirectoryAt :: OptionalHeader -> Int -> Maybe DataDirectory
dataDirectoryAt h i = do
    dataDir <- safeAt i $ dataDirectories h
    if isNullDataDir dataDir
        then Nothing
        else Just dataDir

-- | 'DataDirectory' denotes a data block within an executable. PE/COFF image
-- comes with a set of optional, predefined data directories. This structure
-- identifies each of these predefined entries. See 'dataDirectory'.
data WhichDataDirectory 
    = ExportTable
    | ImportTable
    | ResourceTable
    | ExceptionTable
    | CertificateTable
    | BaseRelocationTable
    | Debug
    | Architecture
    | GlobalPtr
    | TlsTable
    | LoadConfigTable
    | BoundImport
    | ImportAddressTable
    | DelayImportDescriptor
    | ClrRuntimeHeader
    deriving (Show, Eq)

-- | Obtains requested non-null data directory.
-- See https://docs.microsoft.com/en-us/windows/desktop/debug/pe-format#optional-header-data-directories-image-only
dataDirectory :: OptionalHeader -> WhichDataDirectory -> Maybe DataDirectory
dataDirectory header which = dataDirectoryAt header $ case which of
    ExportTable           ->  0
    ImportTable           ->  1
    ResourceTable         ->  2
    ExceptionTable        ->  3
    CertificateTable      ->  4
    BaseRelocationTable   ->  5
    Debug                 ->  6
    Architecture          ->  7
    GlobalPtr             ->  8
    TlsTable              ->  9
    LoadConfigTable       -> 10
    BoundImport           -> 11
    ImportAddressTable    -> 12
    DelayImportDescriptor -> 13
    ClrRuntimeHeader      -> 14