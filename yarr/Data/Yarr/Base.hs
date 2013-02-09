
module Data.Yarr.Base (
    
    Shape,
    
    Dim, Arity, Fun, Vector, VecList,

    NFData(..), deepseq, 
    Regular(..), VecRegular(..),
    
    USource(..),
    UVecSource(..),

    Fusion(..), DefaultFusion(..),

    UTarget(..), Manifest(..), UVecTarget(..)

) where

import Prelude as P

import Control.DeepSeq

import Data.Yarr.Shape as S
import Data.Yarr.Utils.FixedVector as V

import Data.Yarr.Utils.Primitive


class (NFData (UArray r l sh a), Shape sh) => Regular r l sh a where

    data UArray r l sh a
    
    extent :: UArray r l sh a -> sh
    touchArray :: UArray r l sh a -> IO ()
    force :: UArray r l sh a -> IO ()
    force arr = arr `deepseq` return ()
    {-# INLINE force #-}


class (Regular r l sh (v e), Regular slr l sh e, Vector v e) =>
        VecRegular r slr l sh v e | r -> slr where
    slices :: UArray r l sh (v e) -> VecList (Dim v) (UArray slr l sh e)


class Regular r l sh a => USource r l sh a where

    index :: UArray r l sh a -> sh -> IO a
    index arr sh = linearIndex arr $ toIndex (extent arr) sh
    
    linearIndex :: UArray r l sh a -> Int -> IO a
    linearIndex arr i = index arr $ fromIndex (extent arr) i

    {-# INLINE index #-}
    {-# INLINE linearIndex #-}


class (VecRegular r slr l sh v e, USource r l sh (v e), USource slr l sh e) =>
        UVecSource r slr l sh v e



class (USource r l sh a, USource fr l sh b) => Fusion r fr l sh a b where
    fmap :: (a -> b) -> UArray r l sh a -> UArray fr l sh b
    fmap f = fmapM (return . f)
    
    fmapM :: (a -> IO b) -> UArray r l sh a -> UArray fr l sh b

    fzip :: (Arity n, n ~ S n0)
         => Fun n a b -> VecList n (UArray r l sh a) -> UArray fr l sh b
    fzip fun arrs = let funM = P.fmap return fun in fzipM funM arrs

    fzipM :: (Arity n, n ~ S n0)
          => Fun n a (IO b) -> VecList n (UArray r l sh a) -> UArray fr l sh b

    {-# INLINE fmap #-}
    {-# INLINE fzip #-}


class Fusion r fr l sh a b => DefaultFusion r fr l sh a b | r -> fr where
    dmap :: (a -> b) -> UArray r l sh a -> UArray fr l sh b
    dmap = Data.Yarr.Base.fmap
    
    dmapM :: (a -> IO b) -> UArray r l sh a -> UArray fr l sh b
    dmapM = fmapM

    dzip :: (Arity n, n ~ S n0)
         => Fun n a b -> VecList n (UArray r l sh a) -> UArray fr l sh b
    dzip = fzip

    dzipM :: (Arity n, n ~ S n0)
          => Fun n a (IO b) -> VecList n (UArray r l sh a) -> UArray fr l sh b
    dzipM = fzipM

    {-# INLINE dmap #-}
    {-# INLINE dmapM #-}
    {-# INLINE dzip #-}
    {-# INLINE dzipM #-}



class Regular tr tl sh a => UTarget tr tl sh a where
    write :: UArray tr tl sh a -> sh -> a -> IO ()
    write tarr sh = linearWrite tarr $ toIndex (extent tarr) sh
    
    linearWrite :: UArray tr tl sh a -> Int -> a -> IO ()
    linearWrite tarr i = write tarr $ fromIndex (extent tarr) i
    
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

class (USource r l sh a, UTarget mr ml sh a) =>
        Manifest r l mr ml sh a | r l -> mr ml, mr ml -> r l where
    new :: sh -> IO (UArray mr ml sh a)
    freeze :: UArray mr ml sh a -> IO (UArray r l sh a)
    thaw :: UArray r l sh a -> IO (UArray mr ml sh a)


class (VecRegular tr tslr tl sh v e,
       UTarget tr tl sh (v e), UTarget tslr tl sh e) =>
        UVecTarget tr tslr tl sh v e