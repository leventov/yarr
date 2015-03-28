{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Yarr.Repr.Separate (
    -- * @Separate@ representation
    SE,
    -- | There is also @Separate@ 'UArray' family constructor,
    -- which isn't presented in the docs because Haddock
    -- doesn't support associated family constructors.
    --
    -- See source of "Data.Yarr.Repr.Separate" module.
    UArray(..),
    
    fromSlices, unsafeMapSlices,
    Data.Yarr.Repr.Separate.convert,

    -- * Element-wise fusion for arrays of vectors
    dmapElems, dmapElemsM,
    dzipElems2, dzipElems2M, dzipElems3, dzipElems3M,
    dzipElems, dzipElemsM,

    -- * Non-injective element-wise fusion
    fmapElems, fmapElemsM,
    fzipElems2, fzipElems2M, fzipElems3, fzipElems3M,
    fzipElems, fzipElemsM
) where

import Prelude as P
import Data.Functor ((<$>))

import Data.Yarr.Base as B
import Data.Yarr.Fusion
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Utils.FixedVector as V hiding ( index )

-- | SEparate meta array representation. Internally SEparate arrays
-- hold vector of it's slices (so, 'slices' is just getter for them).
--
-- Mostly useful for:
--
--  * Separate in memory manifest 'Data.Yarr.F'oreign arrays (\"Unboxed\" arrays
--    in @vector@/@repa@ libraries terms).
-- 
--  * Element-wise vector array fusion (see group of 'dmapElems' functions).
data SE r

instance (Regular r l sh e, Vector v e) => Regular (SE r) l sh (v e) where

    data UArray (SE r) l sh (v e) =
        Separate !sh (VecList (Dim v) (UArray r l sh e))

    extent (Separate sh _) = sh
    touchArray (Separate _ slices) = V.mapM_ touchArray slices
    force (Separate sh slices) = do
        sh `deepseq` return ()
        V.mapM_ force slices

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}
    {-# INLINE force #-}

instance (NFData (UArray r l sh e), Shape sh, Vector v e) =>
        NFData (UArray (SE r) l sh (v e)) where
    rnf (Separate sh slices) = sh `deepseq` slices `deepseq` ()

instance (Regular r l sh e, Shape sh, Vector v e) =>
        VecRegular (SE r) r l sh v e where
    slices (Separate _ slices) = slices
    {-# INLINE slices #-}


instance (USource r l sh e, Vector v e) => USource (SE r) l sh (v e) where
    index (Separate _ slices) sh =
        V.convert <$> V.mapM (\el -> index el sh) slices
    linearIndex (Separate _ slices) i =
        V.convert <$> V.mapM (\el -> linearIndex el i) slices
    {-# INLINE index #-}
    {-# INLINE linearIndex #-}

instance (USource r l sh e, Vector v e) => UVecSource (SE r) r l sh v e

instance (DefaultFusion r D l sh, Fusion (SE r) D l sh) =>
        DefaultFusion (SE r) D l sh

instance (DefaultIFusion r l D SH sh, IFusion (SE r) l D SH sh) =>
        DefaultIFusion (SE r) l D SH sh


-- | Group of @f-...-Elems-@ functions is used internally to define
-- @d-...-Elems-@ functions.
fmapElems
    :: (VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, Fusion slr fslr l sh,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b) -- ^ .
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v2 b)
fmapElems fs = fmapElemsM $ V.map (return .) fs

fmapElemsM
    :: (VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, Fusion slr fslr l sh,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b) -- ^ .
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v2 b)
fmapElemsM fs arr = Separate (extent arr) $ V.zipWith fmapM fs (slices arr)


fzipElems2
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        USource fslr l sh c, Fusion slr fslr l sh, Vector v c)
    => VecList (Dim v) (a -> b -> c) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray (SE fslr) l sh (v c)
fzipElems2 fs arr1 arr2 =
    let fMs = V.map (\f -> \x y -> return (f x y)) fs
    in fzipElems2M fMs arr1 arr2

fzipElems2M
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        USource fslr l sh c, Fusion slr fslr l sh, Vector v c)
    => VecList (Dim v) (a -> b -> IO c) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray (SE fslr) l sh (v c)
fzipElems2M fs arr1 arr2 =
    let sh = intersect (vl_2 (extent arr1) (extent arr2))
        slices1 = slices arr1
        slices2 = slices arr2
        {-# INLINE makeSlice #-}
        makeSlice i f =
            let sl1 = slices1 V.! i
                sl2 = slices2 V.! i
            in fzip2M f sl1 sl2
    in Separate sh $ V.imap makeSlice fs

fzipElems3
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        VecRegular r slr l sh v c, USource slr l sh c,
        USource fslr l sh d, Fusion slr fslr l sh, Vector v d)
    => VecList (Dim v) (a -> b -> c -> d) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray r l sh (v c)
    -> UArray (SE fslr) l sh (v d)
fzipElems3 fs arr1 arr2 arr3 =
    let fMs = V.map (\f -> \x y z -> return (f x y z)) fs
    in fzipElems3M fMs arr1 arr2 arr3

fzipElems3M
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        VecRegular r slr l sh v c, USource slr l sh c,
        USource fslr l sh d, Fusion slr fslr l sh, Vector v d)
    => VecList (Dim v) (a -> b -> c -> IO d) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray r l sh (v c)
    -> UArray (SE fslr) l sh (v d)
fzipElems3M fs arr1 arr2 arr3 =
    let sh = intersect (vl_3 (extent arr1) (extent arr2) (extent arr3))
        slices1 = slices arr1
        slices2 = slices arr2
        slices3 = slices arr3
        {-# INLINE makeSlice #-}
        makeSlice i f =
            let sl1 = slices1 V.! i
                sl2 = slices2 V.! i
                sl3 = slices3 V.! i
            in fzip3M f sl1 sl2 sl3
    in Separate sh $ V.imap makeSlice fs


fzipElems
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, Fusion slr fslr l sh)
    => VecList (Dim v2) (Fun m a b) -- ^ .
    -> VecList m (UArray r l sh (v a))
    -> UArray (SE fslr) l sh (v2 b)
fzipElems funs arrs =
    let funMs = V.map (P.fmap return) funs
    in fzipElemsM funMs arrs

fzipElemsM
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, Fusion slr fslr l sh)
    => VecList (Dim v2) (Fun m a (IO b)) -- ^ .
    -> VecList m (UArray r l sh (v a))
    -> UArray (SE fslr) l sh (v2 b)
fzipElemsM funs arrs =
    let sh = intersect $ V.map extent arrs
        !allSlices = V.map slices arrs
        {-# INLINE makeSlice #-}
        makeSlice i fun =
            let slices = V.map (V.! i) allSlices
            in fzipM fun slices
    in Separate sh $ V.imap makeSlice funs

{-# INLINE fmapElems #-}
{-# INLINE fmapElemsM #-}
{-# INLINE fzipElems2 #-}
{-# INLINE fzipElems2M #-}
{-# INLINE fzipElems3 #-}
{-# INLINE fzipElems3M #-}
{-# INLINE fzipElems #-}
{-# INLINE fzipElemsM #-}

-- | /O(1)/ Injective element-wise fusion (mapping).
--
-- Example:
--
-- @
-- let domainHSVImage =
--         dmapElems ('vl_3' (* 360) (* 100) (* 100))
--                   normedHSVImage
-- @
--
-- Also, used internally to define 'Data.Yarr.Flow.mapElems' function.
dmapElems
    :: (VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, DefaultFusion slr fslr l sh,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)     -- ^ Vector of mapper functions
    -> UArray r l sh (v a)          -- ^ Source array of vectors
    -> UArray (SE fslr) l sh (v2 b) -- ^ Fused array
dmapElems = fmapElems

-- | /O(1)/ Monadic vesion of 'dmapElems' function.
dmapElemsM
    :: (VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, DefaultFusion slr fslr l sh,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)  -- ^ Elemen-wise vector of monadic mappers
    -> UArray r l sh (v a)          -- ^ Source array of vectors
    -> UArray (SE fslr) l sh (v2 b) -- ^ Result array
dmapElemsM = fmapElemsM


dzipElems2
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        USource fslr l sh c, DefaultFusion slr fslr l sh, Vector v c)
    => VecList (Dim v) (a -> b -> c) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray (SE fslr) l sh (v c)
dzipElems2 = fzipElems2

dzipElems2M
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        USource fslr l sh c, DefaultFusion slr fslr l sh, Vector v c)
    => VecList (Dim v) (a -> b -> IO c) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray (SE fslr) l sh (v c)
dzipElems2M = fzipElems2M

dzipElems3
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        VecRegular r slr l sh v c, USource slr l sh c,
        USource fslr l sh d, DefaultFusion slr fslr l sh, Vector v d)
    => VecList (Dim v) (a -> b -> c -> d) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray r l sh (v c)
    -> UArray (SE fslr) l sh (v d)
dzipElems3 = fzipElems3

dzipElems3M
    :: (VecRegular r slr l sh v a, USource slr l sh a,
        VecRegular r slr l sh v b, USource slr l sh b,
        VecRegular r slr l sh v c, USource slr l sh c,
        USource fslr l sh d, DefaultFusion slr fslr l sh, Vector v d)
    => VecList (Dim v) (a -> b -> c -> IO d) -- ^ .
    -> UArray r l sh (v a)
    -> UArray r l sh (v b)
    -> UArray r l sh (v c)
    -> UArray (SE fslr) l sh (v d)
dzipElems3M = fzipElems3M



-- | /O(1)/ Generalized element-wise zipping of several arrays of vectors.
dzipElems
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, DefaultFusion slr fslr l sh)
    => VecList (Dim v2) (Fun m a b)    -- ^ Vector of wrapped @m-@ary element-wise zippers
    -> VecList m (UArray r l sh (v a)) -- ^ Vector of source arrays of vectors
    -> UArray (SE fslr) l sh (v2 b)    -- ^ Fused result array
dzipElems = fzipElems

-- | /O(1)/ Generalized monadic element-wise zipping of several arrays of vectors
dzipElemsM
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a,
        USource slr l sh a, USource fslr l sh b, DefaultFusion slr fslr l sh)
    => VecList (Dim v2) (Fun m a (IO b)) -- ^ Vector of wrapped @m-@ary
                                         -- element-wise monadic zippers
    -> VecList m (UArray r l sh (v a))   -- ^ Vector of source arrays of vectors
    -> UArray (SE fslr) l sh (v2 b)      -- ^ Result array
dzipElemsM = fzipElemsM

{-# INLINE dmapElems #-}
{-# INLINE dmapElemsM #-}
{-# INLINE dzipElems2 #-}
{-# INLINE dzipElems2M #-}
{-# INLINE dzipElems3 #-}
{-# INLINE dzipElems3M #-}
{-# INLINE dzipElems #-}
{-# INLINE dzipElemsM #-}




instance (UTarget tr tl sh e, Vector v e) => UTarget (SE tr) tl sh (v e) where
    write (Separate _ slices) sh v =
        V.zipWithM_ (\el x -> write el sh x) slices (V.convert v)
    linearWrite (Separate _ slices) i v =
        V.zipWithM_ (\el x -> linearWrite el i x) slices (V.convert v)
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance (Manifest r mr l sh e, Vector v e) =>
        Manifest (SE r) (SE mr) l sh (v e) where
    new sh = P.fmap (Separate sh) (V.replicateM (B.new sh))
    freeze (Separate sh mslices) = P.fmap (Separate sh) (V.mapM freeze mslices)
    thaw (Separate sh slices) = P.fmap (Separate sh) (V.mapM thaw slices)
    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}

instance (UTarget tr tl sh e, Vector v e) => UVecTarget (SE tr) tr tl sh v e

-- | /O(1)/ Glues several arrays of the same type
-- into one separate array of vectors.
-- All source arrays must be of the same extent.
--
-- Example:
--
-- @let separateCoords = fromSlices ('vl_3' xs ys zs)@
fromSlices
    :: (Regular r l sh e, Vector v e, Dim v ~ S n0)
    => VecList (Dim v) (UArray r l sh e)
    -> UArray (SE r) l sh (v e)
{-# INLINE fromSlices #-}
fromSlices slices =
    let shapes = V.map extent slices
        sh0 = V.head shapes
    in if V.any (/= sh0) shapes
            then error "Separate Repr: all slices must be of the same extent"
            else Separate sh0 slices

-- | /O(depends on mapper function)/
-- Maps slices of separate array \"entirely\".
-- 
-- This function is useful when operation over slices is not
-- element-wise (in that case you should use 'Data.Yarr.Flow.mapElems'):
--
-- @let blurredImage = unsafeMapSlices blur image@
--
-- The function is unsafe because it doesn't check that slice mapper
-- translates extents uniformly (though it is pure).
unsafeMapSlices
    :: (USource r l sh a, Vector v a,
        USource r2 l2 sh2 b, Vector v b, Dim v ~ S n0)
    => (UArray r l sh a -> UArray r2 l2 sh2 b)
        -- ^ Slice mapper without restrictions
    -> UArray (SE r) l sh (v a)    -- ^ Source separate array
    -> UArray (SE r2) l2 sh2 (v b) -- ^ Result separate array
{-# INLINE unsafeMapSlices #-}
unsafeMapSlices f (Separate sh slices) =
    let slices' = V.map f slices
    in Separate (extent (V.head slices')) slices'

-- | /O(0)/ Converts separate vector between vector types of the same arity.
--
-- Example:
--
-- @
-- -- floatPairs :: 'UArray' ('SE' 'Data.Yarr.F') 'Dim1' ('VecList' 'N2' Float)
-- let cs :: 'UArray' ('SE' 'Data.Yarr.F') 'Dim1' ('Data.Complex.Complex' Float)
--     cs = convert floatPairs
-- @
convert
    :: (Regular r l sh e, Vector v e, Vector v2 e, Dim v ~ Dim v2)
    => UArray (SE r) l sh (v e) -> UArray (SE r) l sh (v2 e)
{-# INLINE convert #-}
convert (Separate sh slices) = Separate sh slices
