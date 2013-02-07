
module Data.Yarr.Base (
    
    Shape,
    
    Dim, Arity, Fun, Vector, VecList,

    module Control.DeepSeq,
    Regular(..), VecRegular(..),
    entire,
    
    USource(..),
    UVecSource(..),

    Threads, caps, threads,
    Fill,
    ULoad(..), UVecLoad(..), L, SH,
    rangeLoadP, rangeLoadS, rangeLoadSlicesP, rangeLoadSlicesS,

    Fusion(..), DefaultFusion(..),

    UTarget(..), Manifest(..), UVecTarget(..)

) where

import Prelude as P
import GHC.Conc

import Control.DeepSeq

import Data.Yarr.Shape as S
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel
import Data.Yarr.Utils.Primitive


class (NFData (UArray r l sh a), Shape sh) => Regular r l sh a where

    data UArray r l sh a
    
    extent :: UArray r l sh a -> sh
    touch :: UArray r l sh a -> IO ()


class (Regular r l sh (v e), Regular slr l sh e, Vector v e) =>
        VecRegular r slr l sh v e | r -> slr where
    slices :: UArray r l sh (v e) -> VecList (Dim v) (UArray slr l sh e)

entire
    :: (Regular r l sh a, Regular r2 l2 sh b)
    => UArray r l sh a -> UArray r2 l2 sh b -> sh
{-# INLINE entire #-}
entire arr tarr = intersect (vl_2 (extent arr) (extent tarr))

class Regular r l sh a => USource r l sh a where

    index :: UArray r l sh a -> sh -> IO a
    index arr sh = linearIndex arr $ toIndex (extent arr) sh
    
    linearIndex :: UArray r l sh a -> Int -> IO a
    linearIndex arr i = index arr $ fromIndex (extent arr) i

    {-# INLINE index #-}
    {-# INLINE linearIndex #-}


class (VecRegular r slr l sh v e, USource r l sh (v e), USource slr l sh e) =>
        UVecSource r slr l sh v e


type Threads = IO Int

caps :: Threads
caps = getNumCapabilities

threads :: Int -> Threads
{-# INLINE threads #-}
threads = return

class (USource r l sh a, UTarget tr tl sh a) =>
        ULoad r l tr tl sh a where

    type LoadIndex l tl sh

    loadP :: Fill (LoadIndex l tl sh) a
          -> Threads
          -> UArray r l sh a
          -> UArray tr tl sh a
          -> IO ()

    loadS :: Fill (LoadIndex l tl sh) a
          -> UArray r l sh a
          -> UArray tr tl sh a
          -> IO ()

class (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
       ULoad slr l tslr tl sh e, Dim v ~ Dim v2) =>
        UVecLoad r slr l tr tslr tl sh v v2 e where
    loadSlicesP
        :: Fill (LoadIndex l tl sh) e
        -> Threads
        -> UArray r l sh (v e)
        -> UArray tr tl sh (v2 e)
        -> IO ()

    loadSlicesS
        :: Fill (LoadIndex l tl sh) e
        -> UArray r l sh (v e)
        -> UArray tr tl sh (v2 e)
        -> IO ()

data L

instance (USource r L sh a, UTarget tr L sh a) => ULoad r L tr L sh a where
    type LoadIndex L L sh = Int
    loadP = linearLoadP
    loadS = linearLoadS
    {-# INLINE loadP #-}
    {-# INLINE loadS #-}

instance (UVecSource r slr L sh v e, UVecTarget tr tslr L sh v2 e,
          ULoad slr L tslr L sh e, Dim v ~ Dim v2) =>
        UVecLoad r slr L tr tslr L sh v v2 e where
    loadSlicesP = linearLoadSlicesP
    loadSlicesS = linearLoadSlicesS
    {-# INLINE loadSlicesP #-}
    {-# INLINE loadSlicesS #-}

data SH

#define SH_LOAD_INST(l,tl)                                          \
instance (USource r l sh a, UTarget tr tl sh a) => \
        ULoad r l tr tl sh a where { \
    type LoadIndex l tl sh = sh;                                    \
    loadP fill threads arr tarr =                                    \
        rangeLoadP fill threads arr tarr zero (entire arr tarr); \
    loadS fill arr tarr =                                            \
        rangeLoadS fill arr tarr zero (entire arr tarr); \
    {-# INLINE loadP #-};                                            \
    {-# INLINE loadS #-};                                            \
}; \
instance (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e, \
          ULoad slr l tslr tl sh e, Dim v ~ Dim v2) => \
        UVecLoad r slr l tr tslr tl sh v v2 e where { \
    loadSlicesP fill threads arr tarr =                              \
        rangeLoadSlicesP fill threads arr tarr zero (entire arr tarr); \
    loadSlicesS fill arr tarr =                                      \
        rangeLoadSlicesS fill arr tarr zero (entire arr tarr); \
    {-# INLINE loadSlicesP #-};                                      \
    {-# INLINE loadSlicesS #-};                                      \
}

SH_LOAD_INST(SH,L)
SH_LOAD_INST(L,SH)
SH_LOAD_INST(SH,SH)

rangeLoadP
    :: (USource r l sh a, UTarget tr tl sh a)
    => Fill sh a
    -> Threads
    -> UArray r l sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
rangeLoadP fill threads arr tarr start end = do
    ts <- threads
    parallel_ ts $
        makeFork ts start end (fill (index arr) (write tarr))

rangeLoadS
    :: (USource r l sh a, UTarget tr tl sh a)
    => Fill sh a
    -> UArray r l sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
rangeLoadS fill arr tarr start end =
    fill (index arr) (write tarr) start end

linearLoadP
    :: (USource r l sh a, UTarget tr tl sh a)
    => Fill Int a
    -> Threads
    -> UArray r l sh a
    -> UArray tr tl sh a
    -> IO ()
linearLoadP lfill threads arr tarr = do
    ts <- threads
    parallel_ ts $
        makeFork ts 0 (size (extent arr))
             (lfill (linearIndex arr) (linearWrite tarr))

linearLoadS
    :: (USource r l sh a, UTarget tr tl sh a)
    => Fill Int a
    -> UArray r l sh a
    -> UArray tr tl sh a
    -> IO ()
linearLoadS lfill arr tarr =
    lfill (linearIndex arr) (linearWrite tarr) 0 (size (extent arr))

{-# INLINE rangeLoadP #-}
{-# INLINE rangeLoadS #-}
{-# INLINE linearLoadP #-}
{-# INLINE linearLoadS #-}



rangeLoadSlicesP
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => Fill sh e
    -> Threads
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
rangeLoadSlicesP fill threads arr tarr start end = do
    ts <- threads
    parallel_ ts $
        makeForkSlicesOnce
            ts (V.replicate (start, end))
            (V.zipWith
                (\sl tsl -> fill (index sl) (write tsl))
                (slices arr) (slices tarr))

rangeLoadSlicesS
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => Fill sh e
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
rangeLoadSlicesS fill arr tarr start end =
    V.zipWithM_
        (\sl tsl -> rangeLoadS fill sl tsl start end)
        (slices arr) (slices tarr)


linearLoadSlicesP
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => Fill Int e
    -> Threads
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> IO ()
linearLoadSlicesP lfill threads arr tarr = do
    ts <- threads
    parallel_ ts $
        makeForkSlicesOnce
            ts (V.replicate (0, size (extent arr)))
            (V.zipWith
                (\sl tsl -> lfill (linearIndex sl) (linearWrite tsl))
                (slices arr) (slices tarr))

linearLoadSlicesS
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => Fill Int e
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> IO ()
linearLoadSlicesS lfill arr tarr =
    V.zipWithM_ (linearLoadS lfill) (slices arr) (slices tarr)

{-# INLINE rangeLoadSlicesP #-}
{-# INLINE rangeLoadSlicesS #-}
{-# INLINE linearLoadSlicesP #-}
{-# INLINE linearLoadSlicesS #-}



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