
module Data.Yarr.Repr.Foreign (
    F, FS,
    -- | There are also @ForeignArray@  and @ForeignSlice@
    -- 'UArray' family constructors,
    -- which aren't presented in the docs because Haddock
    -- doesn't support associated family constructors.
    --
    -- See source of "Data.Yarr.Repr.Foreign" module.
    UArray(..),

    Storable, L,
    newEmpty,
    toForeignPtr, unsafeFromForeignPtr,
) where

import Control.Monad.IO.Class
import Foreign
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.MissingAlloc

import Data.Yarr.Base as B
import Data.Yarr.Fusion
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Shape

import Data.Yarr.Utils.Storable
import Data.Yarr.Utils.FixedVector as V

-- | Foreign representation is the heart of Yarr framework.
--
-- Internally it holds raw pointer ('Ptr'), which makes indexing
-- foreign arrays not slower than GHC's built-in primitive arrays,
-- but without freeze/thaw boilerplate.
--
-- Foreign arrays are very permissible, for example you can easily
-- use them as source and target of 'Data.Yarr.Eval.Load'ing operation simultaneously,
-- achieving old good in-place @C-@style array modifying:
--
-- @'Data.Yarr.Eval.loadS' 'fill' ('dmap' 'sqrt' arr) arr@
--
-- Foreign arrays are intented to hold all 'Storable' types and
-- vectors of them (because there is a conditional instance of 'Storalbe'
-- class for 'Vector's of 'Storable's too).
data F

instance Shape sh => Regular F L sh a where

    data UArray F L sh a =
        ForeignArray
            !sh              -- Extent
            {-# NOUNPACK #-}
            !(ForeignPtr a)  -- Foreign ptr for GC
            !(Ptr a)         -- Plain ptr for fast memory access

    extent (ForeignArray sh _ _) = sh
    touchArray (ForeignArray _ fptr _) = liftIO $ touchForeignPtr fptr

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}

instance Shape sh => NFData (UArray F L sh a) where
    rnf (ForeignArray sh fptr ptr) = sh `deepseq` fptr `seq` ptr `seq` ()

instance (Shape sh, Storable a) => USource F L sh a where
    linearIndex (ForeignArray _ _ ptr) i = peekElemOff ptr i
    {-# INLINE linearIndex #-}

instance DefaultFusion F D L sh
instance Shape sh => DefaultIFusion F L D SH sh

-- | Foreign Slice representation, /view/ slice representation
-- for 'F'oreign arrays.
--
-- To understand Foreign Slices,
-- suppose you have standard @image@ array of
-- @'UArray' 'F' 'Dim2' ('VecList' 'N3' Word8)@ type.
--
-- It's layout in memory (with array indices):
--
-- @
--  r g b | r g b | r g b | ...
-- (0, 0)  (0, 1)  (0, 2)   ...
-- @
--
-- @
-- let (VecList [reds, greens, blues]) = 'slices' image
-- -- reds, greens, blues :: UArray FS Dim2 Word8
-- @
--
-- Now @blues@ just indexes each third byte on the same underlying
-- memory block:
--
-- @
-- ... b | ... b | ... b | ...
--   (0, 0)  (0, 1)  (0, 2)...
-- @
data FS

instance Shape sh => Regular FS L sh e where

    data UArray FS L sh e =
        ForeignSlice
            !sh              -- Extent
            !Int             -- Size of a vector in the parent array (in bytes)
            {-# NOUNPACK #-}
            !(ForeignPtr e)  -- Foreign ptr for GC
            !(Ptr e)         -- Plain ptr for fast memory access

    extent (ForeignSlice sh _ _ _) = sh
    touchArray (ForeignSlice _ _ fptr _) = liftIO $ touchForeignPtr fptr

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}

instance Shape sh => NFData (UArray FS L sh e) where
    rnf (ForeignSlice sh vsize fptr ptr) =
        sh `deepseq` vsize `seq` fptr `seq` ptr `seq` ()

instance (Shape sh, Storable e) => USource FS L sh e where
    linearIndex (ForeignSlice _ vsize _ ptr) i = peekByteOff ptr (i * vsize)
    {-# INLINE linearIndex #-}

instance DefaultFusion FS D L sh
instance Shape sh => DefaultIFusion FS L D SH sh


instance (Shape sh, Vector v e, Storable e) => VecRegular F FS L sh v e where
    slices (ForeignArray sh fptr ptr) =
        let esize = sizeOf (undefined :: e)
            vsize = sizeOf (undefined :: (v e))
            eptr = castPtr ptr
            feptr = castForeignPtr fptr
        in V.generate $ \i ->
            ForeignSlice sh vsize feptr (eptr `plusPtr` (i * esize))
    {-# INLINE slices #-}

instance (Shape sh, Vector v e, Storable e) => UVecSource F FS L sh v e

instance (Shape sh, Vector v e, Storable e) => UVecSource (SE F) F L sh v e


instance (Shape sh, Storable a) => UTarget F L sh a where
    linearWrite (ForeignArray _ _ ptr) i x = pokeElemOff ptr i x
    {-# INLINE linearWrite #-}

instance (Shape sh, Storable a) => Manifest F F L sh a where
    new sh = do
        arr <- internalNew mallocBytes sh
        arr `deepseq` return ()
        return arr

    freeze = return
    thaw = return

    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}

-- | /O(1)/ allocates zero-initialized foreign array.
--
-- Needed because common 'new' function allocates array with garbage.
newEmpty :: (Shape sh, Storable a, Integral a) => sh -> IO (UArray F L sh a)
{-# INLINE newEmpty #-}
newEmpty sh = do
    arr <- internalNew callocBytes sh
    arr `deepseq` return ()
    return arr

internalNew
    :: forall sh a. (Shape sh, Storable a)
    => (Int -> IO (Ptr a)) -> sh -> IO (UArray F L sh a)
{-# NOINLINE internalNew #-}
internalNew allocBytes sh = do
    let len = size sh
    ptr <- allocBytes (len * sizeOf (undefined :: a))
    fptr <- newForeignPtr finalizerFree (castPtr ptr)
    return $ ForeignArray sh fptr ptr


instance (Shape sh, Storable e) => UTarget FS L sh e where
    linearWrite (ForeignSlice _ vsize _ ptr) i x =
        pokeByteOff ptr (i * vsize) x
    {-# INLINE linearWrite #-}

instance (Shape sh, Vector v e, Storable e) => UVecTarget F FS L sh v e

-- | /O(1)/ Returns pointer to memory block used by the given foreign
-- array.
--
-- May be useful to reuse memory if you don't longer need the given array
-- in the program:
--
-- @
-- brandNewData <-
--    'unsafeFromForeignPtr' ext ('castForeignPtr' (toForeignPtr arr))
-- @
toForeignPtr :: Shape sh => UArray F L sh a -> ForeignPtr a
{-# INLINE toForeignPtr #-}
toForeignPtr (ForeignArray _ fptr _) = fptr

-- | /O(1)/ Wraps foreign ptr into foreign array.
--
-- The function is unsafe because it simply don't (and can't)
-- check anything about correctness of produced array.
unsafeFromForeignPtr :: Shape sh => sh -> ForeignPtr a -> IO (UArray F L sh a)
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr sh fptr =
    withForeignPtr fptr (\ptr -> return $ ForeignArray sh fptr ptr)
