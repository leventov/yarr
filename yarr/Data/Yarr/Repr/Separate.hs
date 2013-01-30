
module Data.Yarr.Repr.Separate where

import Prelude as P
import Data.Functor ((<$>))

import Data.Yarr.Base as B
import Data.Yarr.Repr.Delayed
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


data SE r

instance (URegular r sh e, Vector v e) => URegular (SE r) sh (v e) where

    data UArray (SE r) sh (v e) =
        Separate !sh (VecList (Dim v) (UArray r sh e))

    extent (Separate sh _) = sh
    isReshaped (Separate _ elems) = any isReshaped (toList elems)
    touch (Separate _ elems) = V.mapM_ touch elems

    {-# INLINE extent #-}
    {-# INLINE isReshaped #-}
    {-# INLINE touch #-}

instance (NFData (UArray r sh e), Shape sh, Vector v e) =>
        NFData (UArray (SE r) sh (v e)) where
    rnf (Separate sh elems) = sh `deepseq` elems `deepseq` ()

instance (URegular r sh e, Shape sh, Vector v e) =>
        UVecRegular (SE r) sh r v e where
    elems (Separate _ elems) = elems
    {-# INLINE elems #-}


instance (USource r sh e, Vector v e) => USource (SE r) sh (v e) where
    index (Separate _ elems) sh = convert <$> V.mapM (\el -> index el sh) elems
    linearIndex (Separate _ elems) i =
        convert <$> V.mapM (\el -> linearIndex el i) elems
    {-# INLINE index #-}
    {-# INLINE linearIndex #-}

instance (USource r sh e, Arity n) => UVecSource (SE r) sh r (VecList n) e

instance (USource r sh e, Arity n) => DefaultFusion (SE r) D sh (VecList n e) b



fmapElems
    :: (UVecRegular r sh slr v a, Fusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
fmapElems fs = fmapElemsM $ V.map (return .) fs

fmapElemsM
    :: (UVecRegular r sh slr v a, Fusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
fmapElemsM fs arr = Separate (extent arr) $ V.zipWith fmapM fs (elems arr)

fzipElems
    :: (Vector v2 b, Arity m,
        UVecRegular r sh slr v a, Fusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a b)
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
fzipElems funs arrs =
    let funMs = V.map (P.fmap return) funs
    in fzipElemsM funMs arrs

fzipElemsM
    :: (Vector v2 b, Arity m,
        UVecRegular r sh slr v a, Fusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a (IO b))
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
fzipElemsM funs arrs =
    let sh = intersect $ V.toList $ V.map extent arrs
        !allElems = V.map elems arrs
        {-# INLINE makeElem #-}
        makeElem i _ fun =
            let slices = V.map (V.! i) allElems
            in fzipM fun slices
    in Separate sh $ V.izipWith makeElem funs funs

{-# INLINE fmapElems #-}
{-# INLINE fzipElems #-}
{-# INLINE fmapElemsM #-}
{-# INLINE fzipElemsM #-}


dmapElems
    :: (UVecRegular r sh slr v a, DefaultFusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
dmapElems = fmapElems

dmapElemsM
    :: (UVecRegular r sh slr v a, DefaultFusion slr fslr sh a b,
        Vector v2 b, Dim v ~ Dim v2)
    => VecList (Dim v) (a -> IO b)
    -> UArray r sh (v a)
    -> UArray (SE fslr) sh (v2 b)
dmapElemsM = fmapElemsM

dzipElems
    :: (Vector v2 b, Arity m,
        UVecRegular r sh slr v a, DefaultFusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a b)
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
dzipElems = fzipElems

dzipElemsM
    :: (Vector v2 b, Arity m,
        UVecRegular r sh slr v a, DefaultFusion slr fslr sh a b)
    => VecList (Dim v2) (Fun m a (IO b))
    -> VecList m (UArray r sh (v a))
    -> UArray (SE fslr) sh (v2 b)
dzipElemsM = fzipElemsM

{-# INLINE dmapElems #-}
{-# INLINE dzipElems #-}
{-# INLINE dmapElemsM #-}
{-# INLINE dzipElemsM #-}




instance (UTarget tr sh e, Arity n) => UTarget (SE tr) sh (VecList n e) where
    write (Separate _ elems) sh v =
        V.zipWithM_ (\el x -> write el sh x) elems (convert v)
    linearWrite (Separate _ elems) i v =
        V.zipWithM_ (\el x -> linearWrite el i x) elems (convert v)
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance (Manifest mr sh e, Arity n) => Manifest (SE mr) sh (VecList n e) where
    new sh = (return . Separate sh) =<< (V.replicateM (B.new sh))
    {-# INLINE new #-}

instance (UTarget tr sh e, Arity n) => UVecTarget (SE tr) sh tr (VecList n) e


fromElems
    :: (URegular r sh e, Vector v e)
    => VecList (Dim v) (UArray r sh e)
    -> UArray (SE r) sh (v e)
{-# INLINE fromElems #-}
fromElems elems =
    let shapes = toList $ V.map extent elems
        sh0 = P.head shapes
    in if not $ all (== sh0) shapes
            then error "Separate Repr: all elems must be of the same extent"
            else Separate sh0 elems
