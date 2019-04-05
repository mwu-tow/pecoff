module Data.Pecoff.Enums.Format where

import Data.Binary.Get
import Data.Pecoff.Enum

-- | The PE file format, as contained in 'Data.Pecoff.OptionalHeader.OptionalHeader'. 
data PEFormat = PE32 | ROMImage | PE32Plus deriving (Show, Eq)
instance BinaryRepresentible PEFormat where
    type Representation PEFormat = Word16
instance MyEnum PEFormat where
    mapping = 
        [ (0x10b, PE32)
        , (0x107, ROMImage)
        , (0x20B, PE32Plus)
        ]

type AdressGetter = Get Word64

-- | In a number of places, depending on whether file is PE32+, addresses are encoded using 4 or 8 bytes.
addressGetter :: PEFormat -> AdressGetter
addressGetter PE32Plus = getWord64le
addressGetter _        = fromIntegral <$> getWord32le