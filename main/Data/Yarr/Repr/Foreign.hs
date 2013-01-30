{-# LANGUAGE
    BangPatterns, FlexibleInstances,
    MultiParamTypeClasses, FlexibleContexts,
    TypeFamilies, UndecidableInstances, RankNTypes,
    ScopedTypeVariables, IncoherentInstances #-}

module Data.Yarr.Repr.Foreign where

import Foreign
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Data.Yarr.Base as B
import Data.Yarr.Repr.Delayed
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

instance Shape sh => URegular F sh a where

    data UArray F sh a =
        ForeignArray
            !sh              -- Extent
            {-# NOUNPACK #-}
            !(ForeignPtr a)  -- Foreign ptr for GC
            !(Ptr a)         -- Plain ptr for fast memory access
    
    extent (ForeignArray sh _ _) = sh
    isReshaped _ = False
    touch (ForeignArray _ fptr _) = touchForeignPtr fptr
    
    {-# INLINE extent #-}
    {-# INLINE isReshaped #-}
    {-# INLINE touch #-}    

instance Shape sh => NFData (UArray F sh a) where
    rnf (ForeignArray sh fptr ptr) = sh `deepseq` fptr `seq` ptr `seq` ()

instance (Shape sh, Storable a) => USource F sh a where
    linearIndex (ForeignArray _ _ ptr) i = peekElemOff ptr i
    {-# INLINE linearIndex #-}

instance (Shape sh, Storable a) => DefaultFusion F D sh a b


data FS

instance Shape sh => URegular FS sh e where

    data UArray FS sh e =
        ForeignSlice
            !sh              -- Extent
            !Int             -- Size of a vec in the parent array (in bytes)
            {-# NOUNPACK #-}
            !(ForeignPtr e)  -- Foreign ptr for GC
            !(Ptr e)         -- Plain ptr for fast memory access
    
    extent (ForeignSlice sh _ _ _) = sh
    isReshaped _ = False
    touch (ForeignSlice _ _ fptr _) = touchForeignPtr fptr
    
    {-# INLINE extent #-}
    {-# INLINE isReshaped #-}
    {-# INLINE touch #-}

instance Shape sh => NFData (UArray FS sh e) where
    rnf (ForeignSlice sh vsize fptr ptr) =
        sh `deepseq` vsize `seq` fptr `seq` ptr `seq` ()

instance (Shape sh, Storable e) => USource FS sh e where
    linearIndex (ForeignSlice _ vsize _ ptr) i = peekByteOff ptr (i * vsize)
    {-# INLINE linearIndex #-}

instance (Shape sh, Storable e) => DefaultFusion FS D sh e b


instance (Shape sh, Vector v e, Storable e) => UVecRegular F sh FS v e where
    elems (ForeignArray sh fptr ptr) =
        let esize = sizeOf (undefined :: e)
            vsize = sizeOf (undefined :: (v e))
            eptr = castPtr ptr
            feptr = castForeignPtr fptr
        in V.generate $ \i ->
            ForeignSlice sh vsize feptr (eptr `plusPtr` (i * esize))
    {-# INLINE elems #-}

instance (Shape sh, Vector v e, Storable e) => UVecSource F sh FS v e


instance (Shape sh, Storable a) => UTarget F sh a where
    linearWrite (ForeignArray _ _ ptr) i x = pokeElemOff ptr i x
    {-# INLINE linearWrite #-}

instance (Shape sh, Storable a) => Manifest F sh a where
    new sh = do
        let len = size sh
        ptr <- mallocBytes (len * sizeOf (undefined :: a))
        fptr <- newForeignPtr finalizerFree (castPtr ptr)
        return $ ForeignArray sh fptr ptr
    {-# INLINE new #-}
    

instance (Shape sh, Storable e) => UTarget FS sh e where
    linearWrite (ForeignSlice _ vsize _ ptr) i x =
        pokeByteOff ptr (i * vsize) x
    {-# INLINE linearWrite #-}

instance (Shape sh, Vector v e, Storable e) => UVecTarget F sh FS v e

