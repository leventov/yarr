
-- | Dataflow (fusion operations)
module Data.Yarr.Flow (
    -- * Basic fusion
    DefaultFusion(..), DefaultIFusion(..),

    -- ** 'D'elayed flow and zipping shortcuts
    dzipWith, dzipWith3, D, delay,

    -- * Vector fusion
    SE, dmapElems, dmapElemsM,
    dzipElems2, dzipElems2M, dzipElems3, dzipElems3M,
    dzipElems, dzipElemsM,

    -- * High level shortcuts
    traverse, zipElems, mapElems, mapElemsM
) where

import Data.Yarr.Base
import Data.Yarr.Fusion
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V

-- | /O(1)/ Function from @repa@.
traverse
    :: (USource r l sh a, Shape sh')
    => (sh -> sh')         -- ^ Function to produce result extent
                           -- from source extent.
    -> ((sh -> IO a) -> sh' -> IO b)
                           -- ^ Function to produce elements of result array.
                           -- Passed a lookup function
                           -- to get elements of the source.
    -> UArray r l sh a     -- ^ Source array itself
    -> UArray D SH sh' b   -- ^ Result array
{-# INLINE traverse #-}
traverse transformShape newElem arr =
    ShapeDelayed
        (transformShape (extent arr))
        (touchArray arr) (force arr)
        (newElem (index arr))

-- | /O(1)/ Function for in-place zipping vector elements.
-- 
-- Always true:
--
-- @zipElems f arr == 'dzip' ('Fun' f) ('slices' arr)@
--
-- Example:
--
-- @let φs = zipElems ('flip' 'atan2') coords@
zipElems
    :: (Vector v a,
        USource r l sh (v a), USource fr l sh b, DefaultFusion r fr l sh)
    => Fn (Dim v) a b      -- ^ Unwrapped @n@-ary zipper function
    -> UArray r l sh (v a) -- ^ Source array of vectors
    -> UArray fr l sh b    -- ^ Result array
{-# INLINE zipElems #-}
zipElems fn arr = dmap (\v -> inspect v (Fun fn)) arr

-- | /O(1)/ Maps elements of vectors in array uniformly.
-- Don't confuse with 'dmapElems', which accepts a vector of mapper
-- for each slice.
-- 
-- Typical use case -- type conversion:
--
-- @
-- let floatImage :: UArray F Dim2 Float
--     floatImage = mapElems 'fromIntegral' word8Image
-- @
mapElems
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        USource fslr l sh b, DefaultFusion slr fslr l sh, Vector v b)
    => (a -> b)                     -- ^ Mapper function for all elements
    -> UArray r l sh (v a)          -- ^ Source array of vectors
    -> UArray (SE fslr) l sh (v b)  -- ^ Fused array of vectors
{-# INLINE mapElems #-}
mapElems f = dmapElems (V.replicate f)

-- | /O(1)/ Monadic version of 'mapElems' function.
-- Don't confuse with 'dmapElemsM'.
--
-- Example:
--
-- @let domained = mapElemsM ('Data.Yarr.Utils.Primitive.clampM' 0.0 1.0) floatImage@
mapElemsM
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        USource fslr l sh b, DefaultFusion slr fslr l sh, Vector v b)
    => (a -> IO b)                 -- ^ Monadic mapper for all vector elements
    -> UArray r l sh (v a)         -- ^ Source array of vectors
    -> UArray (SE fslr) l sh (v b) -- ^ Fused array of vectors
{-# INLINE mapElemsM #-}
mapElemsM f = dmapElemsM (V.replicate f)

-- | /O(1)/ Generalized zipping of 2 arrays.
--
-- Main basic \"zipWith\" in Yarr.
--
-- Although sighature of this function has extremely big predicate,
-- it is more permissible than 'dzip2' counterpart, because source arrays
-- shouldn't be of the same type.
--
-- Implemented by means of 'delay' function (source arrays are simply
-- delayed before zipping).
dzipWith
    :: (USource r1 l sh a, DefaultFusion r1 D l sh, USource D l sh a,
        USource r2 l sh b, DefaultFusion r2 D l sh, USource D l sh b,
        USource D l sh c, DefaultFusion D D l sh)
    => (a -> b -> c)    -- ^ Pure zipping function
    -> UArray r1 l sh a -- ^ 1st source array
    -> UArray r2 l sh b -- ^ 2nd source array
    -> UArray D l sh c  -- ^ Fused result array
{-# INLINE dzipWith #-}
dzipWith f arr1 arr2 = dzip2 f (delay arr1) (delay arr2)

-- | /O(1)/ Generalized zipping of 3 arrays, which shouldn't be
-- of the same representation type.
dzipWith3
    :: (USource r1 l sh a, DefaultFusion r1 D l sh, USource D l sh a,
        USource r2 l sh b, DefaultFusion r2 D l sh, USource D l sh b,
        USource r3 l sh c, DefaultFusion r3 D l sh, USource D l sh c,
        USource D l sh d, DefaultFusion D D l sh)
    => (a -> b -> c -> d) -- ^ Pure zipping function
    -> UArray r1 l sh a   -- ^ 1st source array
    -> UArray r2 l sh b   -- ^ 2nd source array
    -> UArray r3 l sh c   -- ^ 3rd source array
    -> UArray D l sh d    -- ^ Result array
{-# INLINE dzipWith3 #-}
dzipWith3 f arr1 arr2 arr3 = dzip3 f (delay arr1) (delay arr2) (delay arr3)