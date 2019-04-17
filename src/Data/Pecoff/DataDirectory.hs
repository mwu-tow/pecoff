module Data.Pecoff.DataDirectory where

import Data.Int

import Data.Pecoff.Gettable
import Data.Pecoff.RVA

-- | 'DataDirectory' denotes a data block within an executable. Used to describe
--   tables in the 'Data.Pecoff.OptionalHeader'.
data DataDirectory = DataDirectory
  { address :: RelativeVirtualAddress
  , size    :: Int32
  }
  deriving (Show, Eq)

-- | PE/COFF optional header may provide a set of optional, predefined data
--   directories. This structure identifies each of these predefined entries.
--   See 'Data.Pecoff.OptionalHeader.dataDirectory'..
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

instance AddressSize DataDirectory where
    addressSize DataDirectory{..} = (address, fromIntegral size)

instance Gettable DataDirectory where
    get = DataDirectory <$> get <*> get

-- | Data Directory is null, if both its RVA and size are set to 0. Null data
--   directories are often used to denote that there is no given entry in the
--   executable.
isNullDataDir :: DataDirectory -> Bool
isNullDataDir DataDirectory{..} = address == nullRva && size == 0