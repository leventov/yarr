
module Data.Yarr.Base (
    module Control.DeepSeq,
    URegular(..), shapeFillPreferred, UVecRegular(..),
    USource(..), UVecSource(..),
    Fusion(..), DefaultFusion(..),
    UTarget(..), Manifest(..), UVecTarget(..)
) where

import Prelude as P
import GHC.Conc

import Control.DeepSeq

import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Touchable


class (NFData (UArray r sh a), Shape sh) => URegular r sh a where

    data UArray r sh a
    
    extent :: UArray r sh a -> sh
    isReshaped :: UArray r sh a -> Bool
    touch :: UArray r sh a -> IO ()

shapeFillPreferred
    :: (URegular r sh a, URegular tr sh b)
    => UArray r sh a -> UArray tr sh b -> Bool
{-# INLINE shapeFillPreferred #-}
shapeFillPreferred arr tarr =
    isReshaped arr || isReshaped tarr || (extent arr) /= (extent tarr)


class (URegular r sh (v e), URegular slr sh e, Vector v e) =>
        UVecRegular r sh slr v e | r -> slr where
    elems :: UArray r sh (v e) -> VecList (Dim v) (UArray slr sh e)




class URegular r sh a => USource r sh a where

    index :: UArray r sh a -> sh -> IO a
    index arr sh = linearIndex arr $ toIndex (extent arr) sh
    
    linearIndex :: UArray r sh a -> Int -> IO a
    linearIndex arr i = index arr $ fromIndex (extent arr) i


    dLoadP :: UTarget tr sh a => UArray r sh a -> UArray tr sh a -> IO ()
    dLoadP arr tarr = do
        threads <- getNumCapabilities
        defaultLoad (rangeLoadP threads) (linearLoadP threads) arr tarr

    rangeLoadP
        :: UTarget tr sh a
        => Int -> UArray r sh a -> UArray tr sh a -> sh -> sh -> IO ()
    rangeLoadP threads arr tarr =
        parallel_ threads (dUnrolledFill (index arr) (write tarr))

    linearLoadP
        :: UTarget tr sh a
        => Int -> UArray r sh a -> UArray tr sh a -> IO ()
    linearLoadP threads arr tarr =
         parallel_
            threads
            (dUnrolledFill (linearIndex arr) (linearWrite tarr))
            0 (size (extent arr))


    dLoadS :: UTarget tr sh a => UArray r sh a -> UArray tr sh a -> IO ()
    dLoadS arr tarr = defaultLoad rangeLoadS linearLoadS arr tarr
    
    rangeLoadS
        :: UTarget tr sh a
        => UArray r sh a -> UArray tr sh a -> sh -> sh -> IO ()
    rangeLoadS arr tarr = dUnrolledFill (index arr) (write tarr)

    linearLoadS
        :: UTarget tr sh a
        => UArray r sh a -> UArray tr sh a -> IO ()
    linearLoadS arr tarr =
        dUnrolledFill
            (linearIndex arr) (linearWrite tarr)
            0 (size (extent arr))
    
    {-# INLINE index #-}
    {-# INLINE linearIndex #-}
    {-# INLINE dLoadP #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE linearLoadP #-}
    {-# INLINE dLoadS #-}
    {-# INLINE rangeLoadS #-}
    {-# INLINE linearLoadS #-}


{-# INLINE defaultLoad #-}
defaultLoad rangeLoad linearLoad arr tarr =
    if shapeFillPreferred arr tarr
        then rangeLoad
                arr tarr
                zero (intersect [extent arr, extent tarr])
        else linearLoad arr tarr



class (UVecRegular r sh slr v e, USource r sh (v e), USource slr sh e) =>
        UVecSource r sh slr v e where

    dLoadElemsP
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
    dLoadElemsP arr tarr = do
        threads <- getNumCapabilities
        defaultLoad
            (rangeLoadElemsP threads) (linearLoadElemsP threads)
            arr tarr

    rangeLoadElemsP
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => Int
        -> UArray r sh (v e) -> UArray tr sh (v2 e)
        -> sh -> sh
        -> IO ()
    rangeLoadElemsP threads arr tarr =
        parallelElems_
            threads
            (V.zipWith
                dUnrolledFill
                (V.map index (elems arr)) (V.map write (elems tarr)))

    linearLoadElemsP
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => Int -> UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
    linearLoadElemsP threads arr tarr =
        parallelElems_
            threads
            (V.zipWith
                dUnrolledFill
                (V.map linearIndex (elems arr))
                (V.map linearWrite (elems tarr)))
            0 (size (extent arr))


    dLoadElemsS
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
    dLoadElemsS arr tarr =
        defaultLoad rangeLoadElemsS linearLoadElemsS arr tarr

    rangeLoadElemsS
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => UArray r sh (v e)
        -> UArray tr sh (v2 e)
        -> sh -> sh
        -> IO ()
    rangeLoadElemsS arr tarr start end =
        V.zipWithM_
            (\sl tsl -> rangeLoadS sl tsl start end)
            (elems arr) (elems tarr)

    linearLoadElemsS
        :: (UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
        => UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
    linearLoadElemsS arr tarr =
        V.zipWithM_ linearLoadS (elems arr) (elems tarr)

    {-# INLINE dLoadElemsP #-}
    {-# INLINE rangeLoadElemsP #-}
    {-# INLINE linearLoadElemsP #-}
    {-# INLINE dLoadElemsS #-}
    {-# INLINE rangeLoadElemsS #-}
    {-# INLINE linearLoadElemsS #-}



class (USource r sh a, USource fr sh b) => Fusion r fr sh a b where
    fmap :: (a -> b) -> UArray r sh a -> UArray fr sh b
    fmap f = fmapM (return . f)
    
    fmapM :: (a -> IO b) -> UArray r sh a -> UArray fr sh b

    fzip
        :: Arity n
        => Fun n a b -> VecList n (UArray r sh a) -> UArray fr sh b
    fzip fun arrs = let funM = P.fmap return fun in fzipM funM arrs

    fzipM
        :: Arity n
        => Fun n a (IO b) -> VecList n (UArray r sh a) -> UArray fr sh b

    {-# INLINE fmap #-}
    {-# INLINE fzip #-}


class Fusion r fr sh a b => DefaultFusion r fr sh a b | r -> fr where
    dmap :: (a -> b) -> UArray r sh a -> UArray fr sh b
    dmap = Data.Yarr.Base.fmap
    
    dmapM :: (a -> IO b) -> UArray r sh a -> UArray fr sh b
    dmapM = fmapM

    dzip
        :: Arity n
        => Fun n a b -> VecList n (UArray r sh a) -> UArray fr sh b
    dzip = fzip

    dzipM
        :: Arity n
        => Fun n a (IO b) -> VecList n (UArray r sh a) -> UArray fr sh b
    dzipM = fzipM

    {-# INLINE dmap #-}
    {-# INLINE dmapM #-}
    {-# INLINE dzip #-}
    {-# INLINE dzipM #-}




class URegular tr sh a => UTarget tr sh a where
    write :: UArray tr sh a -> sh -> a -> IO ()
    write tarr sh = linearWrite tarr $ toIndex (extent tarr) sh
    
    linearWrite :: UArray tr sh a -> Int -> a -> IO ()
    linearWrite tarr i = write tarr $ fromIndex (extent tarr) i
    
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

class UTarget mr sh a => Manifest mr sh a where
    new :: sh -> IO (UArray mr sh a)


class (UVecRegular tr sh tslr v e, UTarget tr sh (v e), UTarget tslr sh e) =>
        UVecTarget tr sh tslr v e