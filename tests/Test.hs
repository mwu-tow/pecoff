module Main where

import Data.Binary.Get
import Data.List
import Data.Pecoff
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
    let s = pSections p
    print $ importTables p
    let baseAddress = imageBase $ optionalHeader $ p
    let Just d@(DataDirectory rva _) = importTableRVA $ optionalHeader $ p
    -- putStrLn $ "Base Address: " <> show baseAddress
    -- putStrLn $ "Import table RVA: " <> show rva
    -- putStrLn $ "Resulting offset: " <> show (fromIntegral rva - baseAddress)
    -- putStrLn $ "Relevant section: " <> show (getSection p rva)
    -- let foffset = rva2Offset rva s
    -- putStrLn $ "File position: " <> show foffset

    -- print $ access d binaryContents p
    let iat = runGet getImportDirectoryTable $ L.fromStrict $ dataDirToBS d s
    print $ iat
    let name = runGet getUtf8String $ L.fromStrict $ B.drop (rvaToFileOffset (nameAddress iat) s) binaryContents
    print name
    -- print $ coffHeader p
    -- print $ optionalHeader p
    -- runTestTT tests
    pure ()
