module Main where

import Data.Binary.Get
import Data.List
import Data.Pecoff
import Data.Pecoff.Gettable
import Data.Pecoff.Imports
import Data.Pecoff.OptionalHeader
import Data.Pecoff.RVA
import Data.Pecoff.Section

import System.IO
import Test.HUnit
import Control.Monad
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

tests = TestList []

provisionalTest :: IO Pecoff
provisionalTest = do 
    binaryContents <- B.readFile "c:/d.dll"
    pure $ parsePecoff binaryContents

main :: IO ()
main = do
    binaryContents <- B.readFile "c:/d.dll"
    let p = parsePecoff binaryContents
    print $ p
    print $ imports p
    -- let s = pSections p
    -- -- print $ importTables parsePecoff
    -- let baseAddress = imageBase $ optionalHeader $ p
    -- let Just d@(DataDirectory rva _) = importTable $ optionalHeader $ p
    -- putStrLn $ "Import table Data Directory: " <> show d
    -- print $ accessSized p d
    -- putStrLn $ "Base Address: " <> show baseAddress
    -- putStrLn $ "Import table RVA: " <> show rva
    -- putStrLn $ "Resulting offset: " <> show (fromIntegral rva - baseAddress)
    -- putStrLn $ "Relevant section: " <> show (getSection p rva)
    -- let foffset = rva2Offset rva s
    -- putStrLn $ "File position: " <> show foffset

    -- print $ access d binaryContents p
    -- let (idt :: [ImportDirectoryEntry]) = runGet get $ L.fromStrict $ accessSized s d
    -- putStrLn $ "IDT: " <> show idt
    -- let idt = getAt p getImportDirectoryEntrys (lookupTable iat)
    -- putStrLn $ "IDT data: " <> show (access p (lookupTable iat))
    -- putStrLn $ "IDT: " <> show idt

    -- let nameFileOffset = case rvaToFileOffset (nameAddress iat) s of
    --         Just offset -> offset
    --         Nothing -> error $ "FAILED TO FIND RVA="<>(nameAddress iat)
    -- let (name::String) = runGet get $ L.fromStrict $ B.drop nameFileOffset binaryContents
    -- print name
    -- print $ coffHeader p
    -- print $ importTables p
    -- runTestTT tests
    pure ()
