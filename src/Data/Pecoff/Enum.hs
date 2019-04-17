module Data.Pecoff.Enum (module X, BinaryRepresentible(..), Enumeration(..), EnumBitField(..), splitBitfield) where

import Data.Bits as X
import Data.Maybe as X
import Data.Word as X
    
import Data.Maybe
import Type.Reflection
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import Data.Pecoff.Gettable

import Debug.Trace

-- | Type function for obtaining underlying binary representation of the type.
class BinaryRepresentible b where
    type Representation b -- FIXME: how can we say that it is FiniteBits ?

-- | Class that represents C-like enumeration type, based on mapping between
--   type values and their binary representations.
class (Typeable e, BinaryRepresentible e, Ord (Representation e)) 
    => Enumeration e where
    enumName :: String -- FIXME: what if I wanted this definition to be conditional on whether e is Typeable? 
    enumName = show $ typeRep @e

    -- | Pairing between binary representation values and values of Enumaration.
    mapping :: [(Representation e, e)]

    -- | Interpret binary value as an Enumeration value.
    asEnum :: Representation e -> Maybe e
    asEnum = flip Map.lookup (Map.fromList mapping)

-- | Enumeration type where many values can be represented using underlying
--   sbitfield storage.
class (BinaryRepresentible e, Enumeration e, Num (Representation e), FiniteBits (Representation e))
    => EnumBitField e where
    -- | Interpret binary value as a bit field containing a set of Enumeration
    --  values.
    asEnums :: Representation e -> [e]
    asEnums = asEnumsDefault
    
    -- | Default implementation for 'asEnums` (so instances can use it when
    --   overriding).
    asEnumsDefault :: Representation e -> [e]
    asEnumsDefault = collectBitField nthBitAsEnum

    -- | Returns Maybe enumeration value for given bit in the bitfield.
    nthBitAsEnum :: Int -> Maybe e
    nthBitAsEnum = asEnum . bit

instance {-# OVERLAPS #-} 
    (EnumBitField e, Gettable (Representation e)) 
    => Gettable [e] where
    get = do
        rep <- get
        pure $ asEnums rep
instance {-# OVERLAPS #-} 
    (Enumeration e, Gettable (Representation e), Show (Representation e)) 
    => Gettable e where
    get = do
        rep <- get
        let mresult = asEnum rep
        case mresult of
            Just e -> pure e
            Nothing -> error $ "wrong "<> enumName @e <> " value: " <> show rep


splitBitfield 
    :: (Bits b) 
    => b -- ^ Input value 
    -> b -- ^ mask
    -> (b, b) -- ^ (bits under mask, other bits)
splitBitfield value mask = (with, without) where
    with    = value .&. mask
    without = value .&. complement mask

-- | Collect values from bitfield by iterating over bits and decoding values
--   using provided function.
collectBitField 
    :: (FiniteBits b) 
    => (Int -> Maybe e) -- ^ Decoder that for given bit value with only single bit set returns enum value
    -> b -- ^ Binary representation of the bitfield
    -> [e]
collectBitField decoder input = concat $ flagFrom <$> indices where
    indices = [1 .. (length - 1)]
    length = finiteBitSize input
    flagFrom n = catMaybes [decoder n | testBit input n]
