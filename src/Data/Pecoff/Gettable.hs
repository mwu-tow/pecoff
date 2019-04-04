module Data.Pecoff.Gettable where

import Data.Binary (encode, Binary)
import Data.Binary.Get (Get, runGet, getWord16le, getWord32le)

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

class Gettable a where
    get :: Get a

    getFrom :: (Binary b) => b -> a
    getFrom b = getLBS (encode b)

    getBS :: B.ByteString -> a
    getBS = runGet get . L.fromStrict 

    getLBS :: L.ByteString -> a
    getLBS = runGet get

instance Gettable Word16 where
    get = getWord16le
instance Gettable Word32 where
    get = getWord32le