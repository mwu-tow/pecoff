module Data.Pecoff.Enums.SectionCharacteristics where

import Data.Pecoff.Enum

-- | Represents the image section header format.
data SectionCharacteristics
    = IMAGE_SCN_TYPE_NO_PAD -- ^ The section should not be padded to the next boundary. This flag is obsolete and is replaced by IMAGE_SCN_ALIGN_1BYTES. This is valid only for object files. 
    | IMAGE_SCN_CNT_CODE -- ^ The section contains executable code. 
    | IMAGE_SCN_CNT_INITIALIZED_DATA -- ^ The section contains initialized data. 
    | IMAGE_SCN_CNT_UNINITIALIZED_DATA -- ^ The section contains uninitialized data. 
    | IMAGE_SCN_LNK_OTHER -- ^ Reserved for future use. 
    | IMAGE_SCN_LNK_INFO  -- ^ The section contains comments or other information. The .drectve section has this type. This is valid for object files only. 
    | IMAGE_SCN_LNK_REMOVE -- ^ The section will not become part of the image. This is valid only for object files. 
    | IMAGE_SCN_LNK_COMDAT -- ^ The section contains COMDAT data. This is valid only for object files. 
    | IMAGE_SCN_GPREL -- ^ The section contains data referenced through the global pointer (GP). 
    | IMAGE_SCN_MEM_PURGEABLE -- ^ Reserved for future use. 
    | IMAGE_SCN_MEM_16BIT -- ^ Reserved for future use. 
    | IMAGE_SCN_MEM_LOCKED -- ^ Reserved for future use. 
    | IMAGE_SCN_MEM_PRELOAD -- ^ Reserved for future use. 
    | IMAGE_SCN_ALIGN_1BYTES -- ^ Align data on a 1-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_2BYTES -- ^ Align data on a 2-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_4BYTES -- ^ Align data on a 4-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_8BYTES -- ^ Align data on an 8-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_16BYTES -- ^ Align data on a 16-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_32BYTES -- ^ Align data on a 32-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_64BYTES -- ^ Align data on a 64-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_128BYTES -- ^ Align data on a 128-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_256BYTES -- ^ Align data on a 256-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_512BYTES -- ^ Align data on a 512-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_1024BYTES -- ^ Align data on a 1024-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_2048BYTES -- ^ Align data on a 2048-byte boundary. Valid only for object files.  
    | IMAGE_SCN_ALIGN_4096BYTES -- ^ Align data on a 4096-byte boundary. Valid only for object files. 
    | IMAGE_SCN_ALIGN_8192BYTES -- ^ Align data on a 8192-byte boundary. Valid only for object files. 
    | IMAGE_SCN_LNK_NRELOC_OVFL -- ^ The section contains extended relocations. 
    | IMAGE_SCN_MEM_DISCARDABLE -- ^ The section can be discarded as needed. 
    | IMAGE_SCN_MEM_NOT_CACHED -- ^ The section cannot be cached. 
    | IMAGE_SCN_MEM_NOT_PAGED -- ^ The section is not pageable.
    | IMAGE_SCN_MEM_SHARED -- ^ The section can be shared in memory.
    | IMAGE_SCN_MEM_EXECUTE -- ^ The section can be executed as code.
    | IMAGE_SCN_MEM_READ -- ^ The section can be read.
    | IMAGE_SCN_MEM_WRITE -- ^ The section can be written to.
    deriving (Show, Eq)
instance BinaryRepresentible SectionCharacteristics where
    type Representation SectionCharacteristics = Word32
instance MyEnum SectionCharacteristics where
    mapping = 
        [ (0x00000008, IMAGE_SCN_TYPE_NO_PAD)
        , (0x00000020, IMAGE_SCN_CNT_CODE )
        , (0x00000040, IMAGE_SCN_CNT_INITIALIZED_DATA)
        , (0x00000080, IMAGE_SCN_CNT_UNINITIALIZED_DATA )
        , (0x00000200, IMAGE_SCN_LNK_INFO)
        , (0x00000800, IMAGE_SCN_LNK_REMOVE)
        , (0x00001000, IMAGE_SCN_LNK_COMDAT)
        , (0x00008000, IMAGE_SCN_GPREL)
        , (0x00100000, IMAGE_SCN_ALIGN_1BYTES)
        , (0x00200000, IMAGE_SCN_ALIGN_2BYTES)
        , (0x00300000, IMAGE_SCN_ALIGN_4BYTES)
        , (0x00400000, IMAGE_SCN_ALIGN_8BYTES)
        , (0x00500000, IMAGE_SCN_ALIGN_16BYTES)
        , (0x00600000, IMAGE_SCN_ALIGN_32BYTES)
        , (0x00700000, IMAGE_SCN_ALIGN_64BYTES)
        , (0x00800000, IMAGE_SCN_ALIGN_128BYTES)
        , (0x00900000, IMAGE_SCN_ALIGN_256BYTES)
        , (0x00A00000, IMAGE_SCN_ALIGN_512BYTES)
        , (0x00B00000, IMAGE_SCN_ALIGN_1024BYTES)
        , (0x00C00000, IMAGE_SCN_ALIGN_2048BYTES)
        , (0x00D00000, IMAGE_SCN_ALIGN_4096BYTES)
        , (0x00E00000, IMAGE_SCN_ALIGN_8192BYTES)
        , (0x01000000, IMAGE_SCN_LNK_NRELOC_OVFL)
        , (0x02000000, IMAGE_SCN_MEM_DISCARDABLE)
        , (0x04000000, IMAGE_SCN_MEM_NOT_CACHED)
        , (0x08000000, IMAGE_SCN_MEM_NOT_PAGED)
        , (0x10000000, IMAGE_SCN_MEM_SHARED)
        , (0x20000000, IMAGE_SCN_MEM_EXECUTE)
        , (0x40000000, IMAGE_SCN_MEM_READ)
        , (0x80000000, IMAGE_SCN_MEM_WRITE)
        ]
instance EnumBitField SectionCharacteristics where
    asEnums input = bitflags <> alignment where
        (alignmentBits, restBits) = splitBitfield input 0x00F00000
        bitflags = asEnumsDefault restBits
        alignment = maybeToList $ asEnum alignmentBits