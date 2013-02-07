
module Data.Yarr.Repr.Foreign where

import Foreign
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Data.Yarr.Base as B
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


instance (Storable e, Vector v e) => Storable (v e) where
    sizeOf _ =
        let esize = sizeOf (undefined :: e)
            n = arity (undefined :: (Dim v))
        in n * esize

    alignment _ = alignment (undefined :: e)

    peek ptr =
        let eptr = castPtr ptr
        in V.generateM (\i -> peekElemOff eptr i)

    poke ptr v =
        let eptr = castPtr ptr
        in imapM_ (\i e -> pokeElemOff eptr i e) v

    {-# INLINE sizeOf #-}
    {-# INLINE alignment #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}


data F

instance Shape sh => Regular F L sh a where

    data UArray F L sh a =
        ForeignArray
            !sh              -- Extent
            {-# NOUNPACK #-}
            !(ForeignPtr a)  -- Foreign ptr for GC
            !(Ptr a)         -- Plain ptr for fast memory access
    
    extent (ForeignArray sh _ _) = sh
    touch (ForeignArray _ fptr _) = touchForeignPtr fptr
    
    {-# INLINE extent #-}
    {-# INLINE touch #-}    

instance Shape sh => NFData (UArray F L sh a) where
    rnf (ForeignArray sh fptr ptr) = sh `deepseq` fptr `seq` ptr `seq` ()

instance (Shape sh, Storable a) => USource F L sh a where
    linearIndex (ForeignArray _ _ ptr) i = peekElemOff ptr i
    {-# INLINE linearIndex #-}

instance (Shape sh, Storable a) => DefaultFusion F D L sh a b


data FS

instance Shape sh => Regular FS L sh e where

    data UArray FS L sh e =
        ForeignSlice
            !sh              -- Extent
            !Int             -- Size of a vec in the parent array (in bytes)
            {-# NOUNPACK #-}
            !(ForeignPtr e)  -- Foreign ptr for GC
            !(Ptr e)         -- Plain ptr for fast memory access
    
    extent (ForeignSlice sh _ _ _) = sh
    touch (ForeignSlice _ _ fptr _) = touchForeignPtr fptr
    
    {-# INLINE extent #-}
    {-# INLINE touch #-}

instance Shape sh => NFData (UArray FS L sh e) where
    rnf (ForeignSlice sh vsize fptr ptr) =
        sh `deepseq` vsize `seq` fptr `seq` ptr `seq` ()

instance (Shape sh, Storable e) => USource FS L sh e where
    linearIndex (ForeignSlice _ vsize _ ptr) i = peekByteOff ptr (i * vsize)
    {-# INLINE linearIndex #-}

instance (Shape sh, Storable e) => DefaultFusion FS D L sh e b


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

instance (Shape sh, Storable a) => Manifest F L F L sh a where
    new sh = do
        let len = size sh
        ptr <- mallocBytes (len * sizeOf (undefined :: a))
        fptr <- newForeignPtr finalizerFree (castPtr ptr)
        return $ ForeignArray sh fptr ptr

    freeze = return
    thaw = return
    
    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}
    

instance (Shape sh, Storable e) => UTarget FS L sh e where
    write tarr@(ForeignSlice ext vsize _ ptr) sh x =
        pokeByteOff ptr ((toIndex ext sh) * vsize) x
    linearWrite (ForeignSlice _ vsize _ ptr) i x =
        pokeByteOff ptr (i * vsize) x
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance (Shape sh, Vector v e, Storable e) => UVecTarget F FS L sh v e


toForeignPtr :: Shape sh => UArray F L sh a -> ForeignPtr a
{-# INLINE toForeignPtr #-}
toForeignPtr (ForeignArray _ fptr _) = fptr

unsafeFromForeignPtr :: Shape sh => sh -> ForeignPtr a -> IO (UArray F L sh a)
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr sh fptr =
    withForeignPtr fptr (\ptr -> return $ ForeignArray sh fptr ptr)