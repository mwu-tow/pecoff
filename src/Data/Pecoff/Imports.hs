module Data.Pecoff.Imports where

import Data.Pecoff.Gettable
import Data.Pecoff.RVA
import Data.Pecoff.Enums.Format

import Data.Time.Clock
import Data.Int
import Data.Word
import Data.Bits
import Data.Binary.Get

import Debug.Trace

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

-- | The import directory table consists of an array of import directory entries, one entry for each DLL to which the image refers.
type ImportDirectoryTable = [ImportDirectoryEntry]

-- | Table is sequence of entries terminated by an entry with null nameAddress RVA.
instance Gettable ImportDirectoryTable where
    get = repeatUntil get $ (== nullRva) . nameAddress

-- | The collection of these entries describes all imports from a given DLL.
data ImportLookupEntry 
    = OrdinalImportLookupEntry Int16 
    | NameImportLookupEntry RelativeVirtualAddress
    deriving (Show, Eq)
 
type ImportLookupTable = [ImportLookupEntry]

data ImportHintNameEntry = ImportHintNameEntry
    { hint :: Word16
    , importName :: String
    }
    deriving (Show, Eq)

instance Gettable ImportHintNameEntry where
    get = ImportHintNameEntry <$> get <*> get

nullImportLookupEntry = NameImportLookupEntry nullRva

importLookupEntry:: (Integral bits, FiniteBits bits) => bits -> ImportLookupEntry
importLookupEntry val = 
    let maskBit = finiteBitSize val - 1
        unmasked = fromIntegral $ clearBit val maskBit
    in if testBit val maskBit
        then OrdinalImportLookupEntry $ fromIntegral $ unmasked
        else NameImportLookupEntry    $ RelativeVirtualAddress $ fromIntegral $ unmasked

repeatUntil :: (Monad m) => m a -> (a -> Bool) -> m [a]
repeatUntil getter stopCondition = do
    value <- getter
    if stopCondition value
        then pure []
        else (value :) <$> repeatUntil getter stopCondition

getImportLookupEntry :: PEFormat -> Get ImportLookupEntry
getImportLookupEntry PE32Plus  = importLookupEntry <$> getWord64le
getImportLookupEntry _ = importLookupEntry <$> getWord32le
  
getImportEntries :: PEFormat -> Get [ImportLookupEntry]
getImportEntries peformat = repeatUntil (getImportLookupEntry peformat) (== nullImportLookupEntry)

data Import = Import
    { libraryName :: String
    , entries     :: [ImportedFunction]
    }
    deriving (Show, Eq)

data ImportedFunction 
    = FunctionName String
    | FunctionOrdinal Int
    deriving (Show, Eq)

resolveLookupEntry :: (Addressable a) => a -> ImportLookupEntry -> ImportedFunction
resolveLookupEntry _ (OrdinalImportLookupEntry ordinal) = FunctionOrdinal $ fromIntegral ordinal
resolveLookupEntry s (NameImportLookupEntry nameHintRva) = 
    let nameHintEntry = getAt s get nameHintRva
    in FunctionName (importName nameHintEntry)

resolveImport :: (Addressable a) => PEFormat -> a -> ImportDirectoryEntry -> Import
resolveImport peformat s idt = 
    let libraryName = getAt s get $ nameAddress idt
        entries = getAt s (getImportEntries peformat) (importAddressTable idt)

    in Import
        { libraryName = libraryName
        , entries = resolveLookupEntry s <$> entries
        }