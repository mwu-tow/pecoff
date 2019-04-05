module Data.Pecoff.Header where

import Data.Pecoff.Gettable
import Data.Int
import Data.Pecoff.Enums
import Data.Time.Clock

-- | Standard COFF File Header
data CoffHeader = CoffHeader
  { machine             :: Machine -- ^ Target machine type
  , sectionCount        :: Int16   -- ^ Number of sections.
  , timestamp           :: UTCTime -- ^ The date and time when the image was created by the linker.
  , symbolTableOffset   :: Int32 -- ^ The offset of the symbol table, in bytes, or zero if no COFF symbol table exists.
  , symbolCount         :: Int32 -- ^ The number of symbols in the symbol table.
  , optionalHeaderSize  :: Int16 -- ^ The size of the optional header, in bytes. This value should be 0 for object files.
  , fileCharacteristics :: [Characteristics]
  } 
  deriving (Show, Eq)

instance Gettable CoffHeader where
    get = do
        machine <- get
        numsect <- get
        tmstamp <- get
        symtoff <- get
        symtcnt <- get
        szopthd <- get
        characteristics <- get
        pure $ CoffHeader
            { machine             = machine
            , sectionCount        = numsect
            , timestamp           = tmstamp
            , symbolTableOffset   = symtoff
            , symbolCount         = symtcnt
            , optionalHeaderSize  = szopthd
            , fileCharacteristics = characteristics
            }
