module Data.Pecoff.Enums.Format where

import Data.Binary.Get
import Data.Pecoff.Enum

-- | The PE file format, as contained in
--   'Data.Pecoff.OptionalHeader.OptionalHeader'. PE32+ images allow for a
--   64-bit address space while limiting the image size to 2 gigabytes. 
data PEFormat = PE32  -- ^ PE32 executable.
              | ROMImage  -- ^ ROM image.
              | PE32Plus  -- ^ PE32+ (64 bit) executable.
    deriving (Show, Eq)

instance BinaryRepresentible PEFormat where
    type Representation PEFormat = Word16
instance Enumeration PEFormat where
    mapping = 
        [ (0x10b, PE32)
        , (0x107, ROMImage)
        , (0x20B, PE32Plus)
        ]

-- | In a number of places, depending on whether file is PE32+, addresses are
--   encoded using 4 or 8 bytes.
addressGetter :: PEFormat -> Get Word64
addressGetter PE32Plus = getWord64le
addressGetter _        = fromIntegral <$> getWord32le