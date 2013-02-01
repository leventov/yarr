
module Data.Yarr.Flow where

import Data.Yarr.Base
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V


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
    :: (VecRegular r sh slr v a, DefaultFusion slr fslr sh a b,
        Vector v b)
    => (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v b)
{-# INLINE mapElems #-}
mapElems = mapElems'

mapElems'
    :: (VecRegular r sh slr v a, DefaultFusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
{-# INLINE mapElems' #-}
mapElems' f = dmapElems (V.replicate f)
