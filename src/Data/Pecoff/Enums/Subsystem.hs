module Data.Pecoff.Enums.Subsystem where

import Data.Pecoff.Enum

-- | The following values defined for the 'Data.Pecoff.Header.subsystem' field of the
--   'Data.Pecoff.Header.OptionalHeader' determine which Windows subsystem (if any) is required to
--   run the image.
data Subsystem
    = IMAGE_SUBSYSTEM_UNKNOWN -- ^ An unknown subsystem.
    | IMAGE_SUBSYSTEM_NATIVE  -- ^ Device drivers and native Windows processes.
    | IMAGE_SUBSYSTEM_WINDOWS_GUI  -- ^ The Windows graphical user interface (GUI) subsystem.
    | IMAGE_SUBSYSTEM_WINDOWS_CUI  -- ^ The Windows character subsystem.
    | IMAGE_SUBSYSTEM_OS2_CUI  -- ^ The OS/2 character subsystem.
    | IMAGE_SUBSYSTEM_POSIX_CUI  -- ^ The Posix character subsystem.
    | IMAGE_SUBSYSTEM_NATIVE_WINDOWS  -- ^ Native Win9x driver.
    | IMAGE_SUBSYSTEM_WINDOWS_CE_GUI  -- ^ Windows CE.
    | IMAGE_SUBSYSTEM_EFI_APPLICATION  -- ^ An Extensible Firmware Interface (EFI) application.
    | IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER  -- ^ An EFI driver with boot services.
    | IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER  -- ^ An EFI driver with run-time services.
    | IMAGE_SUBSYSTEM_EFI_ROM  -- ^ An EFI ROM image.
    | IMAGE_SUBSYSTEM_XBOX  -- ^ XBOX.
    | IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION   -- ^ Windows boot application.  
    deriving (Show, Eq)

instance BinaryRepresentible Subsystem where
    type Representation Subsystem = Word16
instance Enumeration Subsystem where
    mapping = 
        [ ( 0, IMAGE_SUBSYSTEM_UNKNOWN)
        , ( 1, IMAGE_SUBSYSTEM_NATIVE)
        , ( 2, IMAGE_SUBSYSTEM_WINDOWS_GUI)
        , ( 3, IMAGE_SUBSYSTEM_WINDOWS_CUI)
        , ( 5, IMAGE_SUBSYSTEM_OS2_CUI)
        , ( 7, IMAGE_SUBSYSTEM_POSIX_CUI)
        , ( 8, IMAGE_SUBSYSTEM_NATIVE_WINDOWS)
        , ( 9, IMAGE_SUBSYSTEM_WINDOWS_CE_GUI)
        , (10, IMAGE_SUBSYSTEM_EFI_APPLICATION)
        , (11, IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER)
        , (12, IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER)
        , (13, IMAGE_SUBSYSTEM_EFI_ROM)
        , (14, IMAGE_SUBSYSTEM_XBOX)
        , (16, IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION)
        ]
