module Data.Pecoff.Enum (module X, BinaryRepresentible(..), MyEnum(..), EnumBitField(..), splitBitfield) where

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

class BinaryRepresentible b where
    type Representation b -- FIXME: how can we say that it is FiniteBits ?

class (Typeable e, BinaryRepresentible e, Ord (Representation e)) 
    => MyEnum e where
    enumName :: String -- FIXME: what if I wanted this definition to be conditional on whether e is Typeable? 
    enumName = show $ typeRep @e

    mapping :: [(Representation e, e)]
    asEnum :: Representation e -> Maybe e
    asEnum = flip Map.lookup (Map.fromList mapping)

class (BinaryRepresentible e, MyEnum e, Num (Representation e), FiniteBits (Representation e))
    => EnumBitField e where
    asEnums :: Representation e -> [e]
    asEnums = asEnumsDefault
    
    asEnumsDefault :: Representation e -> [e]
    asEnumsDefault = collectBitField nthBitAsEnum

    nthBitAsEnum :: Int -> Maybe e
    nthBitAsEnum = asEnum . bit

instance {-# OVERLAPS #-} (EnumBitField e, Binary.Binary (Representation e)) => Gettable [e] where
    get = do
        rep <- get @(Representation e)
        pure $ asEnums rep
instance {-# OVERLAPS #-} (MyEnum e, Binary.Binary (Representation e), Show (Representation e)) 
    => Gettable e where
    get = do
        rep <- get
        let mresult = asEnum rep
        case mresult of
            Just e -> pure e
            Nothing -> error $ "wrong "<> enumName @e <> " value: " <> show rep

splitBitfield :: (Bits b) => b -> b -> (b, b)
splitBitfield value mask = (with, without) where
    with    = value .&. mask
    without = value .&. complement mask
            
collectBitField 
    :: (FiniteBits b) 
    => (Int -> Maybe e) -- ^ Decoder that for given bit value with only single bit set returns enum value
    -> b -- ^ Binary representation of the bitfield
    -> [e]
collectBitField decoder input = concat $ flagFrom <$> indices where
    indices = [1 .. (length - 1)]
    length = finiteBitSize input
    flagFrom n = catMaybes [decoder n | testBit input n]
