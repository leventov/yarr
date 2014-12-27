
module Data.Yarr.Utils.Primitive where

import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Word
import GHC.Int

import Data.Yarr.Utils.FixedVector as V


-- | Mainly used to fight against GHC simplifier, which gives
-- no chance to LLVM to perform Global Value Numbering optimization.
--
-- Copied from @repa@, see
-- <http://hackage.haskell.org/packages/archive/repa/3.2.3.1/doc/html/Data-Array-Repa-Eval.html>
class Touchable a where
    -- | The function intented to be passed as 3rd parameter
    -- to @unrolled-@ functions in 'Data.Yarr.Shape.Shape' class
    -- and 'Data.Yarr.Shape.dim2BlockFill'.
    --
    -- If your loading operation is strictly local by elements
    -- (in most cases), use 'noTouch' instead of this function.
    touch :: a -> IO ()

instance Touchable Bool where
    touch b = IO (\s -> case touch# b s of s' -> (# s', () #))
    {-# INLINE touch #-}

#define TOUCHABLE_INST(ty,con)                                          \
instance Touchable ty where {                                           \
    touch (con x#) = IO (\s -> case touch# x# s of s' -> (# s', () #)); \
    {-# INLINE touch #-};                                               \
}

TOUCHABLE_INST(Int, I#)
TOUCHABLE_INST(Int8, I8#)
TOUCHABLE_INST(Int16, I16#)
TOUCHABLE_INST(Int32, I32#)
TOUCHABLE_INST(Int64, I64#)
TOUCHABLE_INST(Word, W#)
TOUCHABLE_INST(Word8, W8#)
TOUCHABLE_INST(Word16, W16#)
TOUCHABLE_INST(Word32, W32#)
TOUCHABLE_INST(Word64, W64#)
TOUCHABLE_INST(Float, F#)
TOUCHABLE_INST(Double, D#)

instance (Vector v e, Touchable e) => Touchable (v e) where
    touch = V.mapM_ touch
    {-# INLINE touch #-}

-- | Alias to @(\\_ -> return ())@.
noTouch :: a -> IO ()
{-# INLINE noTouch #-}
noTouch _ = return ()

-- | GHC simplifier tends to float numeric comparsions
-- as high in execution graph as possible, which in conjunction
-- with loop unrolling sometimes leads to dramatic code bloat.
--
-- I'm not sure @-M@ functions work at all,
-- but strict versions defenitely keep comparsions unfloated.
class PrimitiveOrd a where
    -- | Maybe sequential 'min'.
    minM :: a -> a -> IO a
    -- | Definetely sequential 'min'.
    minM' :: a -> a -> IO a
    -- | Maybe sequential 'max'.
    maxM :: a -> a -> IO a
    -- | Definetely sequential 'max'.
    maxM' :: a -> a -> IO a
    -- | Maybe sequential clamp.
    clampM
        :: a    -- ^ Min bound
        -> a    -- ^ Max bound
        -> a    -- ^ Value to clamp
        -> IO a -- ^ Value in bounds

    -- | Definetely sequential clamp.
    clampM'
        :: a    -- ^ Min bound
        -> a    -- ^ Max bound
        -> a    -- ^ Value to clamp
        -> IO a -- ^ Value in bounds

#define PRIM_COMP_INST(ty,con,le,ge)                                 \
instance PrimitiveOrd ty where {                                     \
    minM (con a#) (con b#) =                                         \
        IO (\s -> seq# (con (if isTrue# (le a# b#) then a# else b#)) s);       \
    minM' (con a#) (con b#) =                                        \
        IO (\s ->                                                    \
            let r# = if isTrue# (le a# b#) then a# else b#                     \
            in case touch# r# s of s' -> (# s', (con r#) #));        \
    maxM (con a#) (con b#) =                                         \
        IO (\s -> seq# (con (if isTrue# (ge a# b#) then a# else b#)) s);       \
    maxM' (con a#) (con b#) =                                        \
        IO (\s ->                                                    \
            let r# = if isTrue# (ge a# b#) then a# else b#                     \
            in case touch# r# s of s' -> (# s', (con r#) #));        \
    clampM (con mn#) (con mx#) (con x#) =                            \
        IO (\s -> seq# (con (if isTrue# (le x# mx#)                            \
                                then (if isTrue# (ge x# mn#) then x# else mn#) \
                                else mx#)) s);                       \
    clampM' (con mn#) (con mx#) (con x#) =                           \
        IO (\s -> let r# = if isTrue# (le x# mx#)                              \
                                then (if isTrue# (ge x# mn#) then x# else mn#) \
                                else mx#                             \
                  in case touch# r# s of s' -> (# s', (con r#) #));  \
    {-# INLINE minM #-};                                             \
    {-# INLINE minM' #-};                                            \
    {-# INLINE maxM #-};                                             \
    {-# INLINE maxM' #-};                                            \
    {-# INLINE clampM #-};                                           \
    {-# INLINE clampM' #-};                                          \
}

PRIM_COMP_INST(Int, I#, (<=#), (>=#))
PRIM_COMP_INST(Char, C#, leChar#, geChar#)
PRIM_COMP_INST(Word, W#, leWord#, geWord#)
PRIM_COMP_INST(Double, D#, (<=##), (>=##))
PRIM_COMP_INST(Float, F#, leFloat#, geFloat#)