module Data.Pecoff.Enums.Machine where

import Data.Pecoff.Enum

-- | The 'Data.Pecoff.Header.machine' field of the
--   'Data.Pecoff.Header.CoffHeader' has one of the following values that
--   specifies its CPU type. An image file can be run only on the specified
--   machine or on a system that emulates the specified machine.
data Machine
    = IMAGE_FILE_MACHINE_UNKNOWN -- ^ The contents of this field are assumed to be applicable to any machine type.
    | IMAGE_FILE_MACHINE_AM33  -- ^ Matsushita AM33.
    | IMAGE_FILE_MACHINE_AMD64 -- ^ x64.
    | IMAGE_FILE_MACHINE_ARM -- ^ ARM little endian.
    | IMAGE_FILE_MACHINE_ARM64  -- ^ ARM64 little endian.
    | IMAGE_FILE_MACHINE_ARMNT  -- ^ ARM Thumb-2 little endian.
    | IMAGE_FILE_MACHINE_EBC -- ^ EFI byte code.
    | IMAGE_FILE_MACHINE_I386 -- ^ Intel 386 or later processors and compatible processors.
    | IMAGE_FILE_MACHINE_IA64 -- ^ Intel Itanium processor family.
    | IMAGE_FILE_MACHINE_M32R -- ^ Mitsubishi M32R little endian.
    | IMAGE_FILE_MACHINE_MIPS16 -- ^ MIPS16.
    | IMAGE_FILE_MACHINE_MIPSFPU -- ^ MIPS with FPU.
    | IMAGE_FILE_MACHINE_MIPSFPU16 -- ^ MIPS16 with FPU.
    | IMAGE_FILE_MACHINE_POWERPC -- ^ Power PC little endian.
    | IMAGE_FILE_MACHINE_POWERPCFP -- ^ Power PC with floating point support.
    | IMAGE_FILE_MACHINE_R4000 -- ^ MIPS little endian.
    | IMAGE_FILE_MACHINE_RISCV32 -- ^ RISC-V 32-bit address space.
    | IMAGE_FILE_MACHINE_RISCV64 -- ^ RISC-V 64-bit address space.
    | IMAGE_FILE_MACHINE_RISCV128 -- ^ RISC-V 128-bit address space.
    | IMAGE_FILE_MACHINE_SH3 -- ^ Hitachi SH3.
    | IMAGE_FILE_MACHINE_SH3DSP -- ^ Hitachi SH3 DSP.
    | IMAGE_FILE_MACHINE_SH4 -- ^ Hitachi SH4.
    | IMAGE_FILE_MACHINE_SH5 -- ^ Hitachi SH5.
    | IMAGE_FILE_MACHINE_THUMB -- ^ Thumb.
    | IMAGE_FILE_MACHINE_WCEMIPSV2 -- ^ MIPS little-endian WCE v2.
    deriving (Show, Eq)
instance BinaryRepresentible Machine where
    type Representation Machine = Word16
instance Enumeration Machine where
    mapping = 
        [ (0x0, IMAGE_FILE_MACHINE_UNKNOWN)
        , (0x1d3, IMAGE_FILE_MACHINE_AM33)
        , (0x8664, IMAGE_FILE_MACHINE_AMD64)
        , (0x1c0, IMAGE_FILE_MACHINE_ARM)
        , (0xaa64, IMAGE_FILE_MACHINE_ARM64)
        , (0x1c4, IMAGE_FILE_MACHINE_ARMNT)
        , (0xebc, IMAGE_FILE_MACHINE_EBC)
        , (0x14c, IMAGE_FILE_MACHINE_I386)
        , (0x200, IMAGE_FILE_MACHINE_IA64)
        , (0x9041, IMAGE_FILE_MACHINE_M32R)
        , (0x266, IMAGE_FILE_MACHINE_MIPS16)
        , (0x366, IMAGE_FILE_MACHINE_MIPSFPU)
        , (0x466, IMAGE_FILE_MACHINE_MIPSFPU16)
        , (0x1f0 , IMAGE_FILE_MACHINE_POWERPC)
        , (0x1f1, IMAGE_FILE_MACHINE_POWERPCFP)
        , (0x166, IMAGE_FILE_MACHINE_R4000)
        , (0x5032, IMAGE_FILE_MACHINE_RISCV32)
        , (0x5064, IMAGE_FILE_MACHINE_RISCV64)
        , (0x5128 , IMAGE_FILE_MACHINE_RISCV128)
        , (0x1a2, IMAGE_FILE_MACHINE_SH3)
        , (0x1a3, IMAGE_FILE_MACHINE_SH3DSP)
        , (0x1a6, IMAGE_FILE_MACHINE_SH4)
        , (0x1a8, IMAGE_FILE_MACHINE_SH5)
        , (0x1c2, IMAGE_FILE_MACHINE_THUMB)
        , (0x169, IMAGE_FILE_MACHINE_WCEMIPSV2)
        ]