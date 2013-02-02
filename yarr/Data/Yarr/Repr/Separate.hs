
module Data.Yarr.Repr.Separate where

import Prelude as P
import Data.Functor ((<$>))

import Data.Yarr.Base as B
import Data.Yarr.Repr.Delayed
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


data SE r

instance (Regular r sh e, Vector v e) => Regular (SE r) sh (v e) where

    data UArray (SE r) sh (v e) =
        Separate !sh (VecList (Dim v) (UArray r sh e))

    extent (Separate sh _) = sh
    shapeIndexingPreferred (Separate _ slices) =
        any shapeIndexingPreferred (toList slices)
    touch (Separate _ slices) = V.mapM_ touch slices

    {-# INLINE extent #-}
    {-# INLINE shapeIndexingPreferred #-}
    {-# INLINE touch #-}

instance (NFData (UArray r sh e), Shape sh, Vector v e) =>
        NFData (UArray (SE r) sh (v e)) where
    rnf (Separate sh slices) = sh `deepseq` slices `deepseq` ()

instance (Regular r sh e, Shape sh, Vector v e) =>
        VecRegular (SE r) sh r v e where
    slices (Separate _ slices) = slices
    {-# INLINE slices #-}


instance (USource r sh e, Vector v e) => USource (SE r) sh (v e) where
    index (Separate _ slices) sh = convert <$> V.mapM (\el -> index el sh) slices
    linearIndex (Separate _ slices) i =
        convert <$> V.mapM (\el -> linearIndex el i) slices
    {-# INLINE index #-}
    {-# INLINE linearIndex #-}

instance (USource r sh e, Vector v e) => DefaultFusion (SE r) D sh (v e) b

instance (Shape sh, Vector v e) => UVecSource (SE D) sh D v e


fmapElems
    :: (VecRegular r sh slr v a, Fusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
fmapElems fs = fmapElemsM $ V.map (return .) fs

fmapElemsM
    :: (VecRegular r sh slr v a, Fusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
fmapElemsM fs arr = Separate (extent arr) $ V.zipWith fmapM fs (slices arr)

fzipElems
    :: (Vector v2 b, Arity m,
        VecRegular r sh slr v a, Fusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a b)
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
fzipElems funs arrs =
    let funMs = V.map (P.fmap return) funs
    in fzipElemsM funMs arrs

fzipElemsM
    :: (Vector v2 b, Arity m,
        VecRegular r sh slr v a, Fusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a (IO b))
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
fzipElemsM funs arrs =
    let sh = intersect $ V.toList $ V.map extent arrs
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
    :: (VecRegular r sh slr v a, DefaultFusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
dmapElems = fmapElems

dmapElemsM
    :: (VecRegular r sh slr v a, DefaultFusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
dmapElemsM = fmapElemsM

dzipElems
    :: (Vector v2 b, Arity m,
        VecRegular r sh slr v a, DefaultFusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a b)
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
dzipElems = fzipElems

dzipElemsM
    :: (Vector v2 b, Arity m,
        VecRegular r sh slr v a, DefaultFusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a (IO b))
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
dzipElemsM = fzipElemsM

{-# INLINE dmapElems #-}
{-# INLINE dzipElems #-}
{-# INLINE dmapElemsM #-}
{-# INLINE dzipElemsM #-}




instance (UTarget tr sh e, Vector v e) => UTarget (SE tr) sh (v e) where
    write (Separate _ slices) sh v =
        V.zipWithM_ (\el x -> write el sh x) slices (convert v)
    linearWrite (Separate _ slices) i v =
        V.zipWithM_ (\el x -> linearWrite el i x) slices (convert v)
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance (Manifest r mr sh e, Vector v e) => Manifest (SE r) (SE mr) sh (v e) where
    new sh = P.fmap (Separate sh) (V.replicateM (B.new sh))
    freeze (Separate sh mslices) = P.fmap (Separate sh) (V.mapM freeze mslices)
    thaw (Separate sh slices) = P.fmap (Separate sh) (V.mapM thaw slices)
    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}

instance (UTarget tr sh e, Vector v e) => UVecTarget (SE tr) sh tr v e


fromSlices
    :: (Regular r sh e, Vector v e)
    => VecList (Dim v) (UArray r sh e)
    -> UArray (SE r) sh (v e)
{-# INLINE fromSlices #-}
fromSlices slices =
    let shapes = toList $ V.map extent slices
        sh0 = P.head shapes
    in if not $ all (== sh0) shapes
            then error "Separate Repr: all slices must be of the same extent"
            else Separate sh0 slices

mapSlices
    :: (USource r sh a, Vector v a, USource r2 sh b, Vector v b)
    => (UArray r sh a -> UArray r2 sh b)
    -> UArray (SE r) sh (v a)
    -> UArray (SE r2) sh (v b)
{-# INLINE mapSlices #-}
mapSlices = mapSlices'

mapSlices'
    :: (USource r sh a, Vector v a, USource r2 sh b, Vector v2 b,
        Dim v ~ Dim v2)
    => (UArray r sh a -> UArray r2 sh b)
    -> UArray (SE r) sh (v a)
    -> UArray (SE r2) sh (v2 b)
{-# INLINE mapSlices' #-}
mapSlices' f = fromSlices . V.map f . slices
