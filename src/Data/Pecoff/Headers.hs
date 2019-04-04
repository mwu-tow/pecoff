module Data.Pecoff.Headers where

import Data.Bits
import Data.Word

import Data.Pecoff.Enums
    
imageSubsystem :: Word16 -> Subsystem
imageSubsystem  0 = IMAGE_SUBSYSTEM_UNKNOWN
imageSubsystem  1 = IMAGE_SUBSYSTEM_NATIVE
imageSubsystem  2 = IMAGE_SUBSYSTEM_WINDOWS_GUI
imageSubsystem  3 = IMAGE_SUBSYSTEM_WINDOWS_CUI
imageSubsystem  5 = IMAGE_SUBSYSTEM_OS2_CUI
imageSubsystem  7 = IMAGE_SUBSYSTEM_POSIX_CUI
imageSubsystem  8 = IMAGE_SUBSYSTEM_NATIVE_WINDOWS
imageSubsystem  9 = IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
imageSubsystem 10 = IMAGE_SUBSYSTEM_EFI_APPLICATION
imageSubsystem 11 = IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
imageSubsystem 12 = IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
imageSubsystem 13 = IMAGE_SUBSYSTEM_EFI_ROM
imageSubsystem 14 = IMAGE_SUBSYSTEM_XBOX
imageSubsystem n = error ("hrm " ++ show n)


imageScnCharacteristics :: Word32 -> [SectionCharacteristics]
imageScnCharacteristics n = imageScnAlign_ ((n .&. 0x00f00000) `shiftR` 20) ++ imageScnCharacteristics_ n 32
                          where imageScnAlign_ 0x0 = []
                                imageScnAlign_ 0x1 = [IMAGE_SCN_ALIGN_1BYTES]
                                imageScnAlign_ 0x2 = [IMAGE_SCN_ALIGN_2BYTES]
                                imageScnAlign_ 0x3 = [IMAGE_SCN_ALIGN_4BYTES]
                                imageScnAlign_ 0x4 = [IMAGE_SCN_ALIGN_8BYTES]
                                imageScnAlign_ 0x5 = [IMAGE_SCN_ALIGN_16BYTES]
                                imageScnAlign_ 0x6 = [IMAGE_SCN_ALIGN_32BYTES]
                                imageScnAlign_ 0x7 = [IMAGE_SCN_ALIGN_64BYTES]
                                imageScnAlign_ 0x8 = [IMAGE_SCN_ALIGN_128BYTES]
                                imageScnAlign_ 0x9 = [IMAGE_SCN_ALIGN_256BYTES]
                                imageScnAlign_ 0xa = [IMAGE_SCN_ALIGN_512BYTES]
                                imageScnAlign_ 0xb = [IMAGE_SCN_ALIGN_1024BYTES]
                                imageScnAlign_ 0xc = [IMAGE_SCN_ALIGN_2048BYTES]
                                imageScnAlign_ 0xd = [IMAGE_SCN_ALIGN_4096BYTES]
                                imageScnAlign_ 0xe = [IMAGE_SCN_ALIGN_8192BYTES]
                                imageScnCharacteristics_ n  0 = []
                                imageScnCharacteristics_ n  4 | testBit n  3 = IMAGE_SCN_TYPE_NO_PAD                         : imageScnCharacteristics_ n  3
                                imageScnCharacteristics_ n  6 | testBit n  5 = IMAGE_SCN_CNT_CODE                            : imageScnCharacteristics_ n  5
                                imageScnCharacteristics_ n  7 | testBit n  6 = IMAGE_SCN_CNT_INITIALIZED_DATA                : imageScnCharacteristics_ n  6
                                imageScnCharacteristics_ n  8 | testBit n  7 = IMAGE_SCN_CNT_UNINITIALIZED_DATA              : imageScnCharacteristics_ n  7
                                imageScnCharacteristics_ n  9 | testBit n  8 = IMAGE_SCN_LNK_OTHER                           : imageScnCharacteristics_ n  8
                                imageScnCharacteristics_ n 10 | testBit n  9 = IMAGE_SCN_LNK_INFO                            : imageScnCharacteristics_ n  9
                                imageScnCharacteristics_ n 12 | testBit n 11 = IMAGE_SCN_LNK_REMOVE                          : imageScnCharacteristics_ n 11
                                imageScnCharacteristics_ n 13 | testBit n 12 = IMAGE_SCN_LNK_COMDAT                          : imageScnCharacteristics_ n 12
                                imageScnCharacteristics_ n 16 | testBit n 15 = IMAGE_SCN_GPREL                               : imageScnCharacteristics_ n 15
                                imageScnCharacteristics_ n 18 | testBit n 17 = IMAGE_SCN_MEM_PURGEABLE : IMAGE_SCN_MEM_16BIT : imageScnCharacteristics_ n 17
                                imageScnCharacteristics_ n 19 | testBit n 18 = IMAGE_SCN_MEM_LOCKED                          : imageScnCharacteristics_ n 18
                                imageScnCharacteristics_ n 20 | testBit n 19 = IMAGE_SCN_MEM_PRELOAD                         : imageScnCharacteristics_ n 19
                                imageScnCharacteristics_ n 25 | testBit n 24 = IMAGE_SCN_LNK_NRELOC_OVFL                     : imageScnCharacteristics_ n 24
                                imageScnCharacteristics_ n 26 | testBit n 25 = IMAGE_SCN_MEM_DISCARDABLE                     : imageScnCharacteristics_ n 25
                                imageScnCharacteristics_ n 27 | testBit n 26 = IMAGE_SCN_MEM_NOT_CACHED                      : imageScnCharacteristics_ n 26
                                imageScnCharacteristics_ n 28 | testBit n 27 = IMAGE_SCN_MEM_NOT_PAGED                       : imageScnCharacteristics_ n 27
                                imageScnCharacteristics_ n 29 | testBit n 28 = IMAGE_SCN_MEM_SHARED                          : imageScnCharacteristics_ n 28
                                imageScnCharacteristics_ n 30 | testBit n 29 = IMAGE_SCN_MEM_EXECUTE                         : imageScnCharacteristics_ n 29
                                imageScnCharacteristics_ n 31 | testBit n 30 = IMAGE_SCN_MEM_READ                            : imageScnCharacteristics_ n 30
                                imageScnCharacteristics_ n 32 | testBit n 31 = IMAGE_SCN_MEM_WRITE                           : imageScnCharacteristics_ n 31
                                imageScnCharacteristics_ n i = imageScnCharacteristics_ n (i-1)

imageDllCharacteristics :: Word16 -> [DllCharacteristics]
imageDllCharacteristics n = imageDllCharacteristics_ n 16
                            where imageDllCharacteristics_ n  0 = []
                                  imageDllCharacteristics_ n  7 | testBit n  6 = IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE          : imageDllCharacteristics_ n  6
                                  imageDllCharacteristics_ n  8 | testBit n  7 = IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY       : imageDllCharacteristics_ n  7
                                  imageDllCharacteristics_ n  9 | testBit n  8 = IMAGE_DLL_CHARACTERISTICS_NX_COMPAT             : imageDllCharacteristics_ n  8
                                  imageDllCharacteristics_ n 10 | testBit n  9 = IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION          : imageDllCharacteristics_ n  9
                                  imageDllCharacteristics_ n 11 | testBit n 10 = IMAGE_DLL_CHARACTERISTICS_NO_SEH                : imageDllCharacteristics_ n 10
                                  imageDllCharacteristics_ n 12 | testBit n 11 = IMAGE_DLL_CHARACTERISTICS_NO_BIND               : imageDllCharacteristics_ n 11
                                  imageDllCharacteristics_ n 14 | testBit n 13 = IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER            : imageDllCharacteristics_ n 13
                                  imageDllCharacteristics_ n 16 | testBit n 15 = IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE : imageDllCharacteristics_ n 15
                                  imageDllCharacteristics_ n i = imageDllCharacteristics_ n (i-1)



imageFileCharacteristics :: Word16 -> [Characteristics]
imageFileCharacteristics n = imageFileCharacteristics_ n 16
                            where imageFileCharacteristics_ n  0 = []
                                  imageFileCharacteristics_ n  1 | testBit n  0 = IMAGE_FILE_RELOCS_STRIPPED         : imageFileCharacteristics_ n 0
                                  imageFileCharacteristics_ n  2 | testBit n  1 = IMAGE_FILE_EXECUTABLE_IMAGE        : imageFileCharacteristics_ n 1
                                  imageFileCharacteristics_ n  3 | testBit n  2 = IMAGE_FILE_LINE_NUMS_STRIPPED      : imageFileCharacteristics_ n 2
                                  imageFileCharacteristics_ n  4 | testBit n  3 = IMAGE_FILE_LOCAL_SYMS_STIRPPED     : imageFileCharacteristics_ n 3
                                  imageFileCharacteristics_ n  5 | testBit n  4 = IMAGE_FILE_AGGRESSIVE_WS_TRIM      : imageFileCharacteristics_ n 4
                                  imageFileCharacteristics_ n  6 | testBit n  5 = IMAGE_FILE_LARGE_ADDRESS_AWARE     : imageFileCharacteristics_ n 5
                                  imageFileCharacteristics_ n  8 | testBit n  7 = IMAGE_FILE_BYTES_REVERSED_LO       : imageFileCharacteristics_ n 7
                                  imageFileCharacteristics_ n  9 | testBit n  8 = IMAGE_FILE_32BIT_MACHINE           : imageFileCharacteristics_ n 8
                                  imageFileCharacteristics_ n 10 | testBit n  9 = IMAGE_FILE_DEBUG_STRIPPED          : imageFileCharacteristics_ n 9
                                  imageFileCharacteristics_ n 11 | testBit n 10 = IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP : imageFileCharacteristics_ n 10
                                  imageFileCharacteristics_ n 12 | testBit n 11 = IMAGE_FILE_NET_RUN_FROM_SWAP       : imageFileCharacteristics_ n 11
                                  imageFileCharacteristics_ n 13 | testBit n 12 = IMAGE_FILE_SYSTEM                  : imageFileCharacteristics_ n 12
                                  imageFileCharacteristics_ n 14 | testBit n 13 = IMAGE_FILE_DLL                     : imageFileCharacteristics_ n 13
                                  imageFileCharacteristics_ n 15 | testBit n 14 = IMAGE_FILE_UP_SYSTEM_ONLY          : imageFileCharacteristics_ n 14
                                  imageFileCharacteristics_ n 16 | testBit n 15 = IMAGE_FILE_BYTES_REVERSED_HI       : imageFileCharacteristics_ n 15
                                  imageFileCharacteristics_ n i = imageFileCharacteristics_ n (i-1)


imageFileMachine :: Word16 -> Machine
imageFileMachine 0x0000 = IMAGE_FILE_MACHINE_UNKNOWN
imageFileMachine 0x01d3 = IMAGE_FILE_MACHINE_AM33
imageFileMachine 0x8664 = IMAGE_FILE_MACHINE_AMD64
imageFileMachine 0x01c0 = IMAGE_FILE_MACHINE_ARM
imageFileMachine 0x0ebc = IMAGE_FILE_MACHINE_EBC
imageFileMachine 0x014c = IMAGE_FILE_MACHINE_I386
imageFileMachine 0x0200 = IMAGE_FILE_MACHINE_IA64
imageFileMachine 0x9041 = IMAGE_FILE_MACHINE_M32R
imageFileMachine 0x0266 = IMAGE_FILE_MACHINE_MIPS16
imageFileMachine 0x0366 = IMAGE_FILE_MACHINE_MIPSFPU
imageFileMachine 0x0466 = IMAGE_FILE_MACHINE_MIPSFPU16
imageFileMachine 0x01f0 = IMAGE_FILE_MACHINE_POWERPC
imageFileMachine 0x01f1 = IMAGE_FILE_MACHINE_POWERPCFP
imageFileMachine 0x0166 = IMAGE_FILE_MACHINE_R4000
imageFileMachine 0x01a2 = IMAGE_FILE_MACHINE_SH3
imageFileMachine 0x01a3 = IMAGE_FILE_MACHINE_SH3DSP
imageFileMachine 0x01a6 = IMAGE_FILE_MACHINE_SH4
imageFileMachine 0x01a8 = IMAGE_FILE_MACHINE_SH5
imageFileMachine 0x01c2 = IMAGE_FILE_MACHINE_THUMB
imageFileMachine 0x0169 = IMAGE_FILE_MACHINE_WCEMIPSV2

