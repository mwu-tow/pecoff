module Data.Pecoff.RVA where

import Data.Binary.Get
import Data.Int
import Data.Pecoff.Gettable

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Types that can be used as RVA
class Address a where
    address :: a -> RelativeVirtualAddress

-- | Types denoting data chunks identified by RVA and size
class AddressSize a where
    addressSize :: a -> (RelativeVirtualAddress, Int)

-- | Relative virtual address. In an image file, the address of an item after it
--   is loaded into memory, with the base address of the image file subtracted
--   from it. The RVA of an item almost always differs from its position within
--   the file on disk (file pointer). In an object file, an RVA is less
--   meaningful because memory locations are not assigned. In this case, an RVA
--   would be an address within a section (described later in this table), to
--   which a relocation is later applied during linking. For simplicity, a
--   compiler should just set the first RVA in each section to zero. 
newtype RelativeVirtualAddress = RelativeVirtualAddress { rva :: Int32 }
    deriving (Show, Eq)

instance Gettable RelativeVirtualAddress where
    get = (RelativeVirtualAddress . fromIntegral) <$> getWord32le

instance Address RelativeVirtualAddress where
    address = id

nullRva :: RelativeVirtualAddress
nullRva = RelativeVirtualAddress 0

instance AddressSize (RelativeVirtualAddress, Int) where
    addressSize = id

-- | An entity providing virtual address space that can be accessed through
--   'RelativeVirtualAddress'
class Addressable d where
    {-# MINIMAL access #-}
    access :: d -> RelativeVirtualAddress -> ByteString

    accessSized :: AddressSize ptr => d -> ptr -> ByteString
    accessSized s ptr = B.take size $ access s rva where
        (rva, size) = addressSize ptr

getAt :: (Addressable d, Address a) => d -> Get b -> a -> b
getAt s g a = runGet' g $ access s $ address a

getAt' :: (Addressable d, AddressSize a) => d -> Get b -> a -> b
getAt' s g a = runGet' g $ accessSized s a

-- | like 'runGet' but working on strict 'ByteString'
runGet' :: Get a -> ByteString -> a
runGet' g bs = runGet g $ L.fromStrict bs