module Data.Pecoff.Imports where

import Data.Time.Clock
import Data.Int
import Data.Word
import Data.Bits
import Data.Binary.Get

import Data.Pecoff.Gettable
import Data.Pecoff.RVA
import Data.Pecoff.Enums.Format
import Data.Pecoff.Utils

-- | The import table entry — referencing a single DLL dependency of the image.
data ImportDirectoryEntry = ImportDirectoryEntry
  { lookupTable        :: RelativeVirtualAddress -- ^ The RVA of the import lookup table. This table contains a name or ordinal for each import. 
  , timestamp          :: UTCTime -- ^ The stamp that is set to zero until the image is bound. After the image is bound, this field is set to the time/data stamp of the DLL. 
  , firstForwarder     :: Word32 -- ^ The index of the first forwarder reference.
  , nameAddress        :: RelativeVirtualAddress -- ^ The address of an ASCII string that contains the name of the DLL. This address is relative to the image base. 
  , importAddressTable :: RelativeVirtualAddress -- ^ The RVA of the import address table. The contents of this table are identical to the contents of the import lookup table until the image is bound. 
  }
  deriving (Show, Eq)

instance Gettable ImportDirectoryEntry where
    get = ImportDirectoryEntry <$> get <*> get <*> get <*> get <*> get

-- | The import directory table consists of an array of import directory
--   entries, one entry for each DLL to which the image refers.
type ImportDirectoryTable = [ImportDirectoryEntry]

-- | Table is sequence of entries terminated by an entry with null nameAddress
--   RVA.
instance Gettable ImportDirectoryTable where
    get = repeatUntil get $ (== nullRva) . nameAddress

-- | The collection of these entries describes all imports from a given DLL.
data ImportLookupEntry 
    = OrdinalImportLookupEntry Int16 
    | NameImportLookupEntry RelativeVirtualAddress
    deriving (Show, Eq)
 
-- | Description of all imports from a given DLL.
type ImportLookupTable = [ImportLookupEntry]

-- | Description of the by name import.
data ImportHintNameEntry = ImportHintNameEntry
    { hint :: Word16 -- ^ An index into the export name pointer table. A match is attempted first with this value. If it fails, a binary search is performed on the DLL's export name pointer table. 
    , importName :: String -- ^ An ASCII string that contains the name to import. This is the string that must be matched to the public name in the DLL. This string is case sensitive.
    }
    deriving (Show, Eq)

instance Gettable ImportHintNameEntry where
    get = ImportHintNameEntry <$> get <*> get

-- | Empty entry, terminating the 'ImportLookupTable'.
nullImportLookupEntry :: ImportLookupEntry
nullImportLookupEntry = NameImportLookupEntry nullRva

importLookupEntry :: (Integral bits, FiniteBits bits) => bits -> ImportLookupEntry
importLookupEntry val = 
    let maskBit = finiteBitSize val - 1
        unmasked = clearBit val maskBit
    in if testBit val maskBit
        then OrdinalImportLookupEntry $ fromIntegral $ unmasked
        else NameImportLookupEntry    $ RelativeVirtualAddress $ fromIntegral $ unmasked

getImportLookupEntry :: PEFormat -> Get ImportLookupEntry
getImportLookupEntry PE32Plus  = importLookupEntry <$> getWord64le
getImportLookupEntry _ = importLookupEntry <$> getWord32le
  
getImportEntries :: PEFormat -> Get ImportLookupTable
getImportEntries peformat = repeatUntil (getImportLookupEntry peformat) (== nullImportLookupEntry)

-- | User-friendly description of an imported DLL.
data Import = Import
    { libraryName :: String -- ^ Name of the DLL
    , entries     :: [ImportedSymbol]
    }
    deriving (Show, Eq)

-- | User-friendly description of a symbol imported from DLL.
data ImportedSymbol 
    = FunctionName String -- ^ Import by name
    | FunctionOrdinal Int -- ^ Import by ordinal value
    deriving (Show, Eq)

-- | Convert imported function to a user-friendly form.
resolveLookupEntry :: (Addressable a) => a -> ImportLookupEntry -> ImportedSymbol
resolveLookupEntry _ (OrdinalImportLookupEntry ordinal) = FunctionOrdinal $ fromIntegral ordinal
resolveLookupEntry s (NameImportLookupEntry nameHintRva) = 
    let nameHintEntry = getAt s get nameHintRva
    in FunctionName (importName nameHintEntry)

-- | Translate import dependency description to a user-friendly form.
resolveImport :: (Addressable a) => PEFormat -> a -> ImportDirectoryEntry -> Import
resolveImport peformat s idt = Import{..} where
    libraryName = getAt s get $ nameAddress idt
    entriesToResolve = getAt s (getImportEntries peformat) (lookupTable idt)
    entries = resolveLookupEntry s <$> entriesToResolve

-- | Get pretty description of imports for parsed binary.
getImports :: (AddressSize chunkAddress, Addressable addressable) => PEFormat -> chunkAddress -> addressable -> [Import]
getImports peFormat idtAddress addressable = resolveImport peFormat addressable <$> idts
    where idts = getAt' addressable get idtAddress
