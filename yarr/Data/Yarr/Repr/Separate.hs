
module Data.Yarr.Repr.Separate where

import Prelude as P
import Data.Functor ((<$>))

import Data.Yarr.Base as B
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Utils.FixedVector as V


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

instance (USource r L sh e, Vector v e) => DefaultFusion (SE r) D L sh (v e) b
instance (USource r SH sh e, Vector v e) => DefaultFusion (SE r) D SH sh (v e) b


fmapElems
    :: (VecRegular r slr l sh v a, Fusion slr fslr l sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v2 b)
fmapElems fs = fmapElemsM $ V.map (return .) fs

fmapElemsM
    :: (VecRegular r slr l sh v a, Fusion slr fslr l sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v2 b)
fmapElemsM fs arr = Separate (extent arr) $ V.zipWith fmapM fs (slices arr)

fzipElems
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a, Fusion slr fslr l sh a b)
    => VecList (Dim v2) (Fun m a b)
    -> VecList m (UArray r l sh (v a))
    -> UArray (SE fslr) l sh (v2 b)
fzipElems funs arrs =
    let funMs = V.map (P.fmap return) funs
    in fzipElemsM funMs arrs

fzipElemsM
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a, Fusion slr fslr l sh a b)
    => VecList (Dim v2) (Fun m a (IO b))
    -> VecList m (UArray r l sh (v a))
    -> UArray (SE fslr) l sh (v2 b)
fzipElemsM funs arrs =
    let sh = intersect $ V.map extent arrs
        !allElems = V.map slices arrs
        {-# INLINE makeElem #-}
        makeElem i _ fun =
            let slices = V.map (V.! i) allElems
            in fzipM fun slices
    -- TODO change to V.imap when it will be fixed
    in Separate sh $ V.izipWith makeElem funs funs

{-# INLINE fmapElems #-}
{-# INLINE fzipElems #-}
{-# INLINE fmapElemsM #-}
{-# INLINE fzipElemsM #-}


dmapElems
    :: (VecRegular r slr l sh v a, DefaultFusion slr fslr l sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v2 b)
dmapElems = fmapElems

dmapElemsM
    :: (VecRegular r slr l sh v a, DefaultFusion slr fslr l sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)
    -> UArray r l sh (v a)
    -> UArray (SE fslr) l sh (v2 b)
dmapElemsM = fmapElemsM

dzipElems
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a, DefaultFusion slr fslr l sh a b)
    => VecList (Dim v2) (Fun m a b)
    -> VecList m (UArray r l sh (v a))
    -> UArray (SE fslr) l sh (v2 b)
dzipElems = fzipElems

dzipElemsM
    :: (Vector v2 b, Arity m, m ~ S m0,
        VecRegular r slr l sh v a, DefaultFusion slr fslr l sh a b)
    => VecList (Dim v2) (Fun m a (IO b))
    -> VecList m (UArray r l sh (v a))
    -> UArray (SE fslr) l sh (v2 b)
dzipElemsM = fzipElemsM

{-# INLINE dmapElems #-}
{-# INLINE dzipElems #-}
{-# INLINE dmapElemsM #-}
{-# INLINE dzipElemsM #-}




instance (UTarget tr tl sh e, Vector v e) => UTarget (SE tr) tl sh (v e) where
    write (Separate _ slices) sh v =
        V.zipWithM_ (\el x -> write el sh x) slices (V.convert v)
    linearWrite (Separate _ slices) i v =
        V.zipWithM_ (\el x -> linearWrite el i x) slices (V.convert v)
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance (Manifest r l mr ml sh e, Vector v e) =>
        Manifest (SE r) l (SE mr) ml sh (v e) where
    new sh = P.fmap (Separate sh) (V.replicateM (B.new sh))
    freeze (Separate sh mslices) = P.fmap (Separate sh) (V.mapM freeze mslices)
    thaw (Separate sh slices) = P.fmap (Separate sh) (V.mapM thaw slices)
    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}

instance (UTarget tr tl sh e, Vector v e) => UVecTarget (SE tr) tr tl sh v e


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

unsafeMapSlices
    :: (USource r l sh a, Vector v a,
        USource r2 l2 sh b, Vector v b, Dim v ~ S n0)
    => (UArray r l sh a -> UArray r2 l2 sh b)
    -> UArray (SE r) l sh (v a)
    -> UArray (SE r2) l2 sh (v b)
{-# INLINE unsafeMapSlices #-}
unsafeMapSlices f (Separate sh slices) = Separate sh (V.map f slices)

convert
    :: (Regular r l sh e, Vector v e, Vector v2 e, Dim v ~ Dim v2)
    => UArray (SE r) l sh (v e) -> UArray (SE r) l sh (v2 e)
{-# INLINE convert #-}
convert (Separate sh slices) = Separate sh slices
