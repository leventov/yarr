
module Data.Yarr.Eval (
    Threads, caps, threads,
    Fill,

    Load(..), RangeLoad(..),
    VecLoad(..), RangeVecLoad(..),

    L, SH,

    compute, entire

) where

import GHC.Conc

import Data.Yarr.Base as B
import Data.Yarr.Shape as S

import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel
import Data.Yarr.Utils.Primitive as P


type Threads = IO Int

caps :: Threads
caps = getNumCapabilities

threads :: Int -> Threads
{-# INLINE threads #-}
threads = return


class (USource r l sh a, UTarget tr tl sh a) =>
        Load r l tr tl sh a where

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

class (Load r l tr tl sh a, LoadIndex l tl sh ~ sh) =>
        RangeLoad r l tr tl sh a where
    rangeLoadP
        :: Fill sh a
        -> Threads
        -> UArray r l sh a
        -> UArray tr tl sh a
        -> sh -> sh
        -> IO ()

    rangeLoadS
        :: Fill sh a
        -> UArray r l sh a
        -> UArray tr tl sh a
        -> sh -> sh
        -> IO ()


class (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
       Load slr l tslr tl sh e, Dim v ~ Dim v2) =>
        VecLoad r slr l tr tslr tl sh v v2 e where
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

class (VecLoad r slr l tr tslr tl sh v v2 e, LoadIndex l tl sh ~ sh) =>
        RangeVecLoad r slr l tr tslr tl sh v v2 e where
    rangeLoadSlicesP
        :: Fill sh e
        -> Threads
        -> UArray r l sh (v e)
        -> UArray tr tl sh (v2 e)
        -> sh -> sh
        -> IO ()

    rangeLoadSlicesS
        :: Fill sh e
        -> UArray r l sh (v e)
        -> UArray tr tl sh (v2 e)
        -> sh -> sh
        -> IO ()


data L

instance (USource r L sh a, UTarget tr L sh a) => Load r L tr L sh a where

    type LoadIndex L L sh = Int
    
    loadP lfill threads arr tarr = do
        force arr
        force tarr
        !ts <- threads
        parallel_ ts $
            makeFork ts 0 (size (extent arr))
                     (lfill (linearIndex arr) (linearWrite tarr))
        touchArray arr
        touchArray tarr

    loadS lfill arr tarr = do
        force arr
        force tarr
        lfill (linearIndex arr) (linearWrite tarr) 0 (size (extent arr))
        touchArray arr
        touchArray tarr

    {-# INLINE loadP #-}
    {-# INLINE loadS #-}

instance (UVecSource r slr L sh v e, UVecTarget tr tslr L sh v2 e,
          Load slr L tslr L sh e, Dim v ~ Dim v2) =>
        VecLoad r slr L tr tslr L sh v v2 e where
    loadSlicesP lfill threads arr tarr = do
        force arr
        force tarr
        !ts <- threads
        parallel_ ts $
            makeForkSlicesOnce
                ts (V.replicate (0, size (extent arr)))
                (V.zipWith
                    (\sl tsl -> lfill (linearIndex sl) (linearWrite tsl))
                    (slices arr) (slices tarr))
        touchArray arr
        touchArray tarr

    loadSlicesS lfill arr tarr = do
        force arr
        force tarr
        V.zipWithM_ (loadS lfill) (slices arr) (slices tarr)
        touchArray arr
        touchArray tarr

    {-# INLINE loadSlicesP #-}
    {-# INLINE loadSlicesS #-}


data SH

#define SH_LOAD_INST(l,tl)                                               \
instance (USource r l sh a, UTarget tr tl sh a) =>                       \
        Load r l tr tl sh a where {                                      \
    type LoadIndex l tl sh = sh;                                         \
    loadP fill threads arr tarr =                                        \
        shRangeLoadP fill threads arr tarr zero (entire arr tarr);       \
    loadS fill arr tarr =                                                \
        shRangeLoadS fill arr tarr zero (entire arr tarr);               \
    {-# INLINE loadP #-};                                                \
    {-# INLINE loadS #-};                                                \
};                                                                       \
instance (USource r l sh a, UTarget tr tl sh a) =>                       \
        RangeLoad r l tr tl sh a where {                                 \
    rangeLoadP = shRangeLoadP;                                           \
    rangeLoadS = shRangeLoadS;                                           \
    {-# INLINE rangeLoadP #-};                                           \
    {-# INLINE rangeLoadS #-};                                           \
};                                                                       \
instance (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,      \
          Load slr l tslr tl sh e, Dim v ~ Dim v2) =>                    \
        VecLoad r slr l tr tslr tl sh v v2 e where {                     \
    loadSlicesP fill threads arr tarr =                                  \
        shRangeLoadSlicesP fill threads arr tarr zero (entire arr tarr); \
    loadSlicesS fill arr tarr =                                          \
        shRangeLoadSlicesS fill arr tarr zero (entire arr tarr);         \
    {-# INLINE loadSlicesP #-};                                          \
    {-# INLINE loadSlicesS #-};                                          \
};                                                                       \
instance (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,      \
          Load slr l tslr tl sh e, Dim v ~ Dim v2) =>                    \
        RangeVecLoad r slr l tr tslr tl sh v v2 e where {                \
    rangeLoadSlicesP = shRangeLoadSlicesP;                               \
    rangeLoadSlicesS = shRangeLoadSlicesS;                               \
    {-# INLINE rangeLoadSlicesP #-};                                     \
    {-# INLINE rangeLoadSlicesS #-};                                     \
}

SH_LOAD_INST(SH,L)
SH_LOAD_INST(L,SH)
SH_LOAD_INST(SH,SH)


shRangeLoadP
    :: (USource r l sh a, UTarget tr tl sh a)
    => Fill sh a
    -> Threads
    -> UArray r l sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
{-# INLINE shRangeLoadP #-}
shRangeLoadP fill threads arr tarr start end = do
    force arr
    force tarr
    !ts <- threads
    parallel_ ts $
        makeFork ts start end (fill (index arr) (write tarr))
    touchArray arr
    touchArray tarr

shRangeLoadS
    :: (USource r l sh a, UTarget tr tl sh a)
    => Fill sh a
    -> UArray r l sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
{-# INLINE shRangeLoadS #-}
shRangeLoadS fill arr tarr start end = do
    force arr
    force tarr
    fill (index arr) (write tarr) start end
    touchArray arr
    touchArray tarr


shRangeLoadSlicesP
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => Fill sh e
    -> Threads
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
{-# INLINE shRangeLoadSlicesP #-}
shRangeLoadSlicesP fill threads arr tarr start end = do
    force arr
    force tarr
    !ts <- threads
    parallel_ ts $
        makeForkSlicesOnce
            ts (V.replicate (start, end))
            (V.zipWith
                (\sl tsl -> fill (index sl) (write tsl))
                (slices arr) (slices tarr))
    touchArray arr
    touchArray tarr

shRangeLoadSlicesS
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => Fill sh e
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
{-# INLINE shRangeLoadSlicesS #-}
shRangeLoadSlicesS fill arr tarr start end = do
    force arr
    force tarr
    V.zipWithM_
        (\sl tsl -> shRangeLoadS fill sl tsl start end)
        (slices arr) (slices tarr)
    touchArray arr
    touchArray tarr


compute
    :: (USource r l sh a, Manifest tr tl mtr mtl sh b)
    => (UArray r l sh a -> UArray mtr mtl sh b -> IO ())
    -> UArray r l sh a -> IO (UArray tr tl sh b)
{-# INLINE compute #-}
compute load arr = do
    marr <- new (extent arr)
    load arr marr
    freeze marr


entire :: (Regular r l sh a, Regular r2 l2 sh b)
       => UArray r l sh a -> UArray r2 l2 sh b -> sh
{-# INLINE entire #-}
entire arr tarr = intersect (vl_2 (extent arr) (extent tarr))
