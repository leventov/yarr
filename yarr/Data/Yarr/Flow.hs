
module Data.Yarr.Flow where

import Data.Yarr.Base
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V


traverse
    :: (USource r l sh a, Shape sh')
    => (sh -> sh')
    -> ((sh -> IO a) -> sh' -> IO b)
    -> UArray r l sh a -> UArray D SH sh' b
{-# INLINE traverse #-}
traverse transformShape newElem arr =
    let newSh = transformShape (extent arr)
    in ShapeDelayed newSh (touch arr) (newElem (index arr))


zipElems
    :: (Vector v a, DefaultFusion r fr l sh (v a) b)
    => Fn (Dim v) a b
    -> UArray r l sh (v a)
    -> UArray fr l sh b
{-# INLINE zipElems #-}
zipElems fn arr = dmap (\v -> inspect v (Fun fn)) arr


mapElems
    :: (VecRegular r slr l sh v a, DefaultFusion slr fslr l sh a b,
        Vector v b)
    => (a -> b)
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v b)
{-# INLINE mapElems #-}
mapElems f = dmapElems (V.replicate f)

mapElemsM
    :: (VecRegular r slr l sh v a, DefaultFusion slr fslr l sh a b,
        Vector v b)
    => (a -> IO b)
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v b)
{-# INLINE mapElemsM #-}
mapElemsM f = dmapElemsM (V.replicate f)
