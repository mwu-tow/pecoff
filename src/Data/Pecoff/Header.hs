module Data.Pecoff.Header where

import Data.Pecoff.Gettable
import Data.Int
import Data.Pecoff.Enums
import Data.Time.Clock

-- | Standard COFF File Header
data CoffHeader = CoffHeader
  { machine             :: Machine -- ^ Target machine type.
  , sectionCount        :: Int16   -- ^ Number of sections.
  , timestamp           :: UTCTime -- ^ The date and time when the image was created by the linker.
  , symbolTableOffset   :: Int32 -- ^ The offset of the symbol table, in bytes, or zero if no COFF symbol table exists.
  , symbolCount         :: Int32 -- ^ The number of symbols in the symbol table.
  , optionalHeaderSize  :: Int16 -- ^ The size of the optional header, in bytes. This value should be 0 for object files.
  , fileCharacteristics :: [Characteristics] -- ^ The flags that indicate the attributes of the file. For specific flag values, see 'Characteristics'.
  } 
  deriving (Show, Eq)

instance Gettable CoffHeader where
    get = CoffHeader <$> get <*> get <*> get <*> get <*> get <*> get <*> get