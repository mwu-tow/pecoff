-- | Parses a ByteString into a Pecoff record. Parsing of section data currently
-- left as a todo.
module Data.Pecoff 
(
    -- * PE/COFF parser
    module Data.Pecoff.Pecoff, readPecoff
    -- * Header records
    , module XHeaders
    , module XOptionalHeader
    , module XDataDir
    -- * Enumeration types
    , module XEnums
    -- * Sections
    , module XSection
    , module XSectionCharacteristics
    -- * Imports
    , module XImports
    , imports
    , dependenciesOfBinary
) where

import qualified Data.ByteString as B

import Data.Pecoff.Pecoff (Pecoff(..), parsePecoff)
import Data.Pecoff.Header as XHeaders (CoffHeader(..))
import Data.Pecoff.OptionalHeader as XOptionalHeader (OptionalHeader(..))
import Data.Pecoff.DataDirectory as XDataDir (DataDirectory(..))
import Data.Pecoff.Section as XSection (SectionHeader(..), Section(..), SectionData(..))
import Data.Pecoff.Enums.SectionCharacteristics as XSectionCharacteristics (SectionCharacteristics(..))
import Data.Pecoff.Enums as XEnums (Characteristics(..), DllCharacteristics(..), PEFormat(..), Machine(..), Subsystem(..))
import Data.Pecoff.Imports as XImports (Import(..), ImportedSymbol(..))


import Control.Monad.IO.Class
import Data.Pecoff.DataDirectory
import Data.Pecoff.Imports
import Data.Pecoff.OptionalHeader

-- | Get pretty description of imports for parsed binary.
imports :: Pecoff -> [Import]
imports Pecoff{..} = 
    let importDataDir = dataDirectory optionalHeader ImportTable
    in case importDataDir of 
        Just ddir -> getImports (format optionalHeader) ddir sections
        Nothing -> []

-- | Returns filenames of imported dynamic libraries for binary under given path.
dependenciesOfBinary 
    :: MonadIO m
    => FilePath  -- ^ Path to the PE/COFF binary to be checked.
    -> m [FilePath]
dependenciesOfBinary path = fmap libraryName <$> (imports <$> readPecoff path)

-- | Load and parse PE/COFF file into a Pecoff record.
readPecoff :: MonadIO m => FilePath -> m Pecoff
readPecoff path = parsePecoff <$> liftIO (B.readFile path)
