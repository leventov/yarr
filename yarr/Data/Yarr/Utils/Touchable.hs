
module Data.Yarr.Utils.Touchable where

import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Word
import GHC.Int

import Data.Yarr.Utils.FixedVector as V

class Touchable a where
    touch :: a -> IO ()

instance Touchable Bool where
    touch b = IO (\s -> case touch# b s of s' -> (# s', () #))
    {-# INLINE touch #-}

#define INST(ty,con)                                                    \
instance Touchable ty where {                                           \
    touch (con x#) = IO (\s -> case touch# x# s of s' -> (# s', () #)); \
    {-# INLINE touch #-};                                               \
}

INST(Int,I#)
INST(Int8,I8#)
INST(Int16,I16#)
INST(Int32,I32#)
INST(Int64,I64#)
INST(Word,W#)
INST(Word8,W8#)
INST(Word16,W16#)
INST(Word32,W32#)
INST(Word64,W64#)
INST(Float,F#)
INST(Double,D#)

instance (Vector v e, Touchable e) => Touchable (v e) where
    touch = V.mapM_ touch
    {-# INLINE touch #-}

noTouch :: a -> IO ()
{-# INLINE noTouch #-}
noTouch _ = return ()