module Data.Pecoff.Enums.Characteristics where

import Data.Pecoff.Enum

-- | The 'Data.Pecoff.Header.fileCharacteristics' field of the 'Data.Pecoff.Header.CoffHeader' contains flags that
-- indicate attributes of the object or image file. The following flags are
-- currently defined:
data Characteristics
    = IMAGE_FILE_RELOCS_STRIPPED -- ^ Image only, Windows CE, and Microsoft Windows NT and later. This indicates that the file does not contain base relocations and must therefore be loaded at its preferred base address. If the base address is not available, the loader reports an error. The default behavior of the linker is to strip base relocations from executable (EXE) files. 
    | IMAGE_FILE_EXECUTABLE_IMAGE -- ^ Image only. This indicates that the image file is valid and can be run (there are no unresolved external references). If this flag is not set, it indicates a linker error. 
    | IMAGE_FILE_LINE_NUMS_STRIPPED -- ^ COFF line numbers have been removed. This flag is deprecated and should be zero. 
    | IMAGE_FILE_LOCAL_SYMS_STIRPPED -- ^ COFF symbol table entries for local symbols have been removed. This flag is deprecated and should be zero. 
    | IMAGE_FILE_AGGRESSIVE_WS_TRIM -- ^ Obsolete. Aggressively trim working set. This flag is deprecated for Windows 2000 and later and must be zero. 
    | IMAGE_FILE_LARGE_ADDRESS_AWARE -- ^ Application can handle > 2-GB addresses. 
    | IMAGE_FILE_BYTES_REVERSED_LO -- ^ Little endian: the least significant bit (LSB) precedes the most significant bit (MSB) in memory. This flag is deprecated and should be zero. 
    | IMAGE_FILE_32BIT_MACHINE -- ^ Machine is based on a 32-bit-word architecture. 
    | IMAGE_FILE_DEBUG_STRIPPED -- ^ Debugging information is removed from the image file. 
    | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP -- ^ If the image is on removable media, fully load it and copy it to the swap file. 
    | IMAGE_FILE_NET_RUN_FROM_SWAP -- ^ If the image is on network media, fully load it and copy it to the swap file. 
    | IMAGE_FILE_SYSTEM -- ^ The image file is a system file, not a user program. 
    | IMAGE_FILE_DLL -- ^ The image file is a dynamic-link library (DLL). Such files are considered executable files for almost all purposes, although they cannot be directly run. 
    | IMAGE_FILE_UP_SYSTEM_ONLY -- ^ The file should be run only on a uniprocessor machine. 
    | IMAGE_FILE_BYTES_REVERSED_HI -- ^ Big endian: the MSB precedes the LSB in memory. This flag is deprecated and should be zero. 
    deriving (Show, Eq)
instance BinaryRepresentible Characteristics where
    type Representation Characteristics = Word16
instance MyEnum Characteristics where
    mapping = 
        [ (0x0001, IMAGE_FILE_RELOCS_STRIPPED)
        , (0x0002, IMAGE_FILE_EXECUTABLE_IMAGE)
        , (0x0004, IMAGE_FILE_LINE_NUMS_STRIPPED)
        , (0x0008, IMAGE_FILE_LOCAL_SYMS_STIRPPED)
        , (0x0010, IMAGE_FILE_AGGRESSIVE_WS_TRIM)
        , (0x0020, IMAGE_FILE_LARGE_ADDRESS_AWARE)
        , (0x0080, IMAGE_FILE_BYTES_REVERSED_LO)
        , (0x0100, IMAGE_FILE_32BIT_MACHINE)
        , (0x0200, IMAGE_FILE_DEBUG_STRIPPED)
        , (0x0400, IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP)
        , (0x0800, IMAGE_FILE_NET_RUN_FROM_SWAP)
        , (0x1000, IMAGE_FILE_SYSTEM)
        , (0x2000, IMAGE_FILE_DLL)
        , (0x4000, IMAGE_FILE_UP_SYSTEM_ONLY)
        , (0x8000, IMAGE_FILE_BYTES_REVERSED_HI)
        ]
instance EnumBitField Characteristics