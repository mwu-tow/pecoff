module Data.Pecoff.Enums.DllCharacteristics where

import Data.Pecoff.Enum

-- | The following values are defined for the 'dllCharactertics' field of the
-- 'OptionalHeader'.
data DllCharacteristics
    = IMAGE_DLL_CHARACTERISTICS_HIGH_ENTROPY_VA  -- ^ Image can handle a high entropy 64-bit virtual address space. 
    | IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE -- ^ DLL can be relocated at load time. 
    | IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY -- ^ Code Integrity checks are enforced. 
    | IMAGE_DLL_CHARACTERISTICS_NX_COMPAT -- ^ Image is NX compatible. 
    | IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION -- ^ Isolation aware, but do not isolate the image. 
    | IMAGE_DLL_CHARACTERISTICS_NO_SEH -- ^ Does not use structured exception (SE) handling. No SE handler may be called in this image. 
    | IMAGE_DLL_CHARACTERISTICS_NO_BIND -- ^ Do not bind the image. 
    | IMAGE_DLL_CHARACTERISTICS_APPCONTAINER -- ^ Image must execute in an AppContainer. 
    | IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER -- ^ A WDM driver. 
    | IMAGE_DLL_CHARACTERISTICS_GUARD_CF -- ^ Image supports Control Flow Guard. 
    | IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE -- ^ Terminal Server aware. 
    deriving (Show, Eq)
instance BinaryRepresentible DllCharacteristics where
    type Representation DllCharacteristics = Word16
instance MyEnum DllCharacteristics where
    mapping = 
        [ (0x0020, IMAGE_DLL_CHARACTERISTICS_HIGH_ENTROPY_VA)
        , (0x0040, IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE)
        , (0x0080, IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY)
        , (0x0100, IMAGE_DLL_CHARACTERISTICS_NX_COMPAT)
        , (0x0200, IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION)
        , (0x0400, IMAGE_DLL_CHARACTERISTICS_NO_SEH)
        , (0x0800, IMAGE_DLL_CHARACTERISTICS_NO_BIND)
        , (0x1000, IMAGE_DLL_CHARACTERISTICS_APPCONTAINER)
        , (0x2000, IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER)
        , (0x4000, IMAGE_DLL_CHARACTERISTICS_GUARD_CF)
        , (0x8000, IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE)
        ]
instance EnumBitField DllCharacteristics