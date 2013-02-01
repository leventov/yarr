
module Data.Yarr.Base (
    module Control.DeepSeq,
    Shape,
    Dim, Arity, Fun, Vector, VecList,
    Regular(..), VecRegular(..),
    USource(..), UVecSource(..),
    Fusion(..), DefaultFusion(..),
    UTarget(..), Manifest(..), UVecTarget(..)
) where

import Prelude as P

import Control.DeepSeq

import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork


class (NFData (UArray r sh a), Shape sh) => Regular r sh a where

    data UArray r sh a
    
    extent :: UArray r sh a -> sh
    shapeIndexingPreferred :: UArray r sh a -> Bool
    touch :: UArray r sh a -> IO ()


class (Regular r sh (v e), Regular slr sh e, Vector v e) =>
        VecRegular r sh slr v e | r -> slr where
    slices :: UArray r sh (v e) -> VecList (Dim v) (UArray slr sh e)



class Regular r sh a => USource r sh a where

    index :: UArray r sh a -> sh -> IO a
    index arr sh = linearIndex arr $ toIndex (extent arr) sh
    
    linearIndex :: UArray r sh a -> Int -> IO a
    linearIndex arr i = index arr $ fromIndex (extent arr) i


    rangeLoadP
        :: UTarget tr sh a
        => Int -> UArray r sh a -> UArray tr sh a -> sh -> sh -> IO ()
    rangeLoadP threads arr tarr =
        parallel_ threads (dUnrolledFill (index arr) (write tarr))

    rangeLoadS
        :: UTarget tr sh a
        => UArray r sh a -> UArray tr sh a -> sh -> sh -> IO ()
    rangeLoadS arr tarr = dUnrolledFill (index arr) (write tarr)


    linearLoadP
        :: UTarget tr sh a
        => Int -> UArray r sh a -> UArray tr sh a -> IO ()
    linearLoadP threads arr tarr =
         parallel_
            threads
            (dUnrolledFill (linearIndex arr) (linearWrite tarr))
            0 (size (extent arr))

    linearLoadS
        :: UTarget tr sh a
        => UArray r sh a -> UArray tr sh a -> IO ()
    linearLoadS arr tarr =
        dUnrolledFill
            (linearIndex arr) (linearWrite tarr)
            0 (size (extent arr))
    
    {-# INLINE index #-}
    {-# INLINE linearIndex #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE rangeLoadS #-}
    {-# INLINE linearLoadP #-}
    {-# INLINE linearLoadS #-}



class (VecRegular r sh slr v e, USource r sh (v e), USource slr sh e) =>
        UVecSource r sh slr v e where

    rangeLoadSlicesP
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => Int
        -> UArray r sh (v e) -> UArray tr sh (v2 e)
        -> sh -> sh
        -> IO ()
    rangeLoadSlicesP threads arr tarr =
        parallelSlices_
            threads
            (V.zipWith
                dUnrolledFill
                (V.map index (slices arr)) (V.map write (slices tarr)))

    rangeLoadSlicesS
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => UArray r sh (v e)
        -> UArray tr sh (v2 e)
        -> sh -> sh
        -> IO ()
    rangeLoadSlicesS arr tarr start end =
        V.zipWithM_
            (\sl tsl -> rangeLoadS sl tsl start end)
            (slices arr) (slices tarr)


    linearLoadSlicesP
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => Int -> UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
    linearLoadSlicesP threads arr tarr =
        parallelSlices_
            threads
            (V.zipWith
                dUnrolledFill
                (V.map linearIndex (slices arr))
                (V.map linearWrite (slices tarr)))
            0 (size (extent arr))

    linearLoadSlicesS
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
    linearLoadSlicesS arr tarr =
        V.zipWithM_ linearLoadS (slices arr) (slices tarr)

    {-# INLINE rangeLoadSlicesP #-}
    {-# INLINE linearLoadSlicesP #-}
    {-# INLINE rangeLoadSlicesS #-}
    {-# INLINE linearLoadSlicesS #-}



class (USource r sh a, USource fr sh b) => Fusion r fr sh a b where
    fmap :: (a -> b) -> UArray r sh a -> UArray fr sh b
    fmap f = fmapM (return . f)
    
    fmapM :: (a -> IO b) -> UArray r sh a -> UArray fr sh b

    fzip :: Arity n
         => Fun n a b -> VecList n (UArray r sh a) -> UArray fr sh b
    fzip fun arrs = let funM = P.fmap return fun in fzipM funM arrs

    fzipM :: Arity n
          => Fun n a (IO b) -> VecList n (UArray r sh a) -> UArray fr sh b

    {-# INLINE fmap #-}
    {-# INLINE fzip #-}


class Fusion r fr sh a b => DefaultFusion r fr sh a b | r -> fr where
    dmap :: (a -> b) -> UArray r sh a -> UArray fr sh b
    dmap = Data.Yarr.Base.fmap
    
    dmapM :: (a -> IO b) -> UArray r sh a -> UArray fr sh b
    dmapM = fmapM

    dzip :: Arity n
         => Fun n a b -> VecList n (UArray r sh a) -> UArray fr sh b
    dzip = fzip

    dzipM :: Arity n
          => Fun n a (IO b) -> VecList n (UArray r sh a) -> UArray fr sh b
    dzipM = fzipM

    {-# INLINE dmap #-}
    {-# INLINE dmapM #-}
    {-# INLINE dzip #-}
    {-# INLINE dzipM #-}




class Regular tr sh a => UTarget tr sh a where
    write :: UArray tr sh a -> sh -> a -> IO ()
    write tarr sh = linearWrite tarr $ toIndex (extent tarr) sh
    
    linearWrite :: UArray tr sh a -> Int -> a -> IO ()
    linearWrite tarr i = write tarr $ fromIndex (extent tarr) i
    
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

class UTarget mr sh a => Manifest mr sh a where
    new :: sh -> IO (UArray mr sh a)


class (VecRegular tr sh tslr v e, UTarget tr sh (v e), UTarget tslr sh e) =>
        UVecTarget tr sh tslr v e