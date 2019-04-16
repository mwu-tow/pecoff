module Data.Pecoff.Gettable (Gettable(..)) where

import Data.Binary (encode, Binary)
import Data.Binary.Get (Get, runGet, getWord8, getWord16le, getWord32le, getWord64le, isEmpty)
import Control.Monad
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Bits
import Data.Char (chr)
import Data.Int
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Class for datatypes that can be binary deserialized from a PE/COFF file.
class Gettable a where
    -- | Action that deserializes value
    get :: Get a

    -- getFrom :: (Binary b) => b -> a
    -- getFrom b = getLBS (encode b)

    -- | Loads value from strict 'B.ByteString'
    getBS :: B.ByteString -> a
    getBS = runGet get . L.fromStrict 

    -- | Loads value from lazy 'B.ByteString'
    getLBS :: L.ByteString -> a
    getLBS = runGet get

instance Gettable Word8 where
    get = getWord8
instance Gettable Word16 where
    get = getWord16le
instance Gettable Word32 where
    get = getWord32le
instance Gettable Word64 where
    get = getWord64le

instance Gettable Int8 where
    get = liftM fromIntegral getWord8
instance Gettable Int16 where
    get = liftM fromIntegral getWord16le
instance Gettable Int32 where
    get = liftM fromIntegral getWord32le
instance Gettable Int64 where
    get = liftM fromIntegral getWord64le

-- | Deserialize as 4-byte second count from epoch start
instance Gettable UTCTime where
    get = (posixSecondsToUTCTime . fromInteger . fromIntegral) <$> getWord32le

-- | Deserialize UTF-8 encoded character (taking 1 - 4 bytes)
instance Gettable Char where
    get = do
        let getByte = liftM fromIntegral getWord8 :: Get Word32
        b1 <- getByte
        let getCharUTF82 = do
                b2 <- getByte
                if b2 .&. 0xc0 == 0x80 
                    then return $ ((b1 .&. 0x1f) `shiftL` 6) 
                               .|. (b2 .&. 0x3f)
                    else fail "Invalid second byte in UTf8 string."
            getCharUTF83 = do
                b2 <- getByte
                b3 <- getByte
                if b2 .&. 0xc0 == 0x80 && b3 .&. 0xc0 == 0x80 
                    then return $ ((b1 .&. 0x0f) `shiftL` 12)
                              .|. ((b2 .&. 0x3f) `shiftL` 6) 
                              .|.  (b3 .&. 0x3f)
                    else fail "Invalid second or third byte in UTf8 string."
            getCharUTF84 = do
                b2 <- getByte
                b3 <- getByte
                b4 <- getByte
                if b2 .&. 0xc0 == 0x80 && b3 .&. 0xc0 == 0x80 && b4 .&. 0xc0 == 0x80 
                    then return $ ((b1 .&. 0x07) `shiftL` 18)
                              .|. ((b2 .&. 0x3f) `shiftL` 12)
                              .|. ((b3 .&. 0x3f) `shiftL` 6) 
                              .|.  (b4 .&. 0x3f)
                    else fail "Invalid second or third or fourth byte in UTf8 string."
        codePoint <- case b1 of
            n | n .&. 0x80 == 0x00 -> return n
            n | n .&. 0xe0 == 0xc0 -> getCharUTF82
            n | n .&. 0xf0 == 0xe0 -> getCharUTF83
            n | n .&. 0xf8 == 0xf0 -> getCharUTF84
            _                      -> fail "Invalid first byte in UTF8 string."
        pure $ chr $ fromIntegral codePoint
    

-- | Deserialize UTF-8 encoded string, terminated by null character or input end
instance Gettable String where
    get = do
        empty <- isEmpty
        if empty 
            then pure []
            else do
                char <- get
                if char == '\0'
                    then pure []
                    else (char :) <$> get
