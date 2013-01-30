{-# LANGUAGE
    TypeFamilies, MultiParamTypeClasses, FlexibleContexts,
    FlexibleInstances, UndecidableInstances,
    OverlappingInstances, FunctionalDependencies,
    GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}

module Data.Yarr.Flow where

import Data.Yarr.Base
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V

safeFill
    :: (USource r sh a, UTarget tr sh b)
    => (UArray r sh a -> UArray tr sh b -> IO ())
    -> UArray tr sh b
    -> UArray r sh a
    -> IO ()
{-# INLINE safeFill #-}
safeFill load tarr arr = do
    load arr tarr
    touch arr
    touch tarr

fillP
    :: (USource r sh a, UTarget tr sh a)
    => UArray tr sh a
    -> UArray r sh a
    -> IO ()
{-# INLINE fillP #-}
fillP = safeFill dLoadP

fillS
    :: (USource r sh a, UTarget tr sh a)
    => UArray tr sh a
    -> UArray r sh a
    -> IO ()
{-# INLINE fillS #-}
fillS = safeFill dLoadS


safeCompute
    :: (USource r sh a, Manifest mr sh b)
    => (UArray r sh a -> UArray mr sh b -> IO ())
    -> UArray r sh a
    -> IO (UArray mr sh b)
{-# INLINE safeCompute #-}
safeCompute load arr = do
    marr <- new (extent arr)
    safeFill load marr arr
    return marr

computeP
    :: (USource r sh a, Manifest mr sh a)
    => UArray r sh a
    -> IO (UArray mr sh a)
{-# INLINE computeP #-}
computeP = safeCompute dLoadP

computeS
    :: (USource r sh a, Manifest mr sh a)
    => UArray r sh a
    -> IO (UArray mr sh a)
{-# INLINE computeS #-}
computeS = safeCompute dLoadS


computeElemsP
    :: (UVecSource r sh slr v1 a,
        Manifest mr sh (v2 a), UVecTarget mr sh mslr v2 a,
        Dim v1 ~ Dim v2)
    => (UArray r sh (v1 a)) -> IO (UArray mr sh (v2 a))
{-# INLINE computeElemsP #-}
computeElemsP = safeCompute dLoadElemsP


traverse
    :: (USource r sh a, Shape sh')
    => (sh -> sh')
    -> ((sh -> IO a) -> sh' -> IO b)
    -> (UArray r sh a) -> UArray D sh' b
{-# INLINE traverse #-}
traverse transformShape newElem arr =
    let newSh = transformShape (extent arr)
    in fromShapeFunction newSh (touch arr) (newElem (index arr))


zipElems
    :: (Vector v a, DefaultFusion r fr sh (v a) b)
    => Fn (Dim v) a b
    -> UArray r sh (v a)
    -> UArray fr sh b
{-# INLINE zipElems #-}
zipElems fn arr = dmap (\v -> inspect v (Fun fn)) arr


mapElems
    :: (UVecRegular r sh slr v a, DefaultFusion slr fslr sh a b)
    => (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (VecList (Dim v) b)
{-# INLINE mapElems #-}
mapElems f = dmapElems (V.replicate f)
