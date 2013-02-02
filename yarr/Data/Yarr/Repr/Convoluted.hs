
module Data.Yarr.Repr.Convoluted where

import Prelude as P
import Control.Monad
import GHC.Conc

import Data.Yarr.Base as B
import Data.Yarr.Shape as S
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork as Fork
import Data.Yarr.Utils.Touchable as T
import Data.Yarr.Utils.Parallel as Par
import Data.Yarr.Utils.Split


data CV

instance Shape sh => Regular CV sh a where

    data UArray CV sh a =
        Convoluted {
            getExtent :: !sh,
            getTouch :: (IO ()),
            borderGet :: (sh -> IO a),
            center :: !(sh, sh),
            centerGet :: (sh -> IO a)}

    extent = getExtent
    shapeIndexingPreferred _ = True
    touch = getTouch

    {-# INLINE extent #-}
    {-# INLINE shapeIndexingPreferred #-}
    {-# INLINE touch #-}


instance Shape sh => NFData (UArray CV sh a) where
    rnf (Convoluted sh tch bget center cget) =
        sh `deepseq` tch `seq` bget `seq` center `deepseq` cget `seq` ()
    {-# INLINE rnf #-}



instance USource CV Dim2 a where
    index (Convoluted _ _ bget center cget) sh =
        if insideBlock center sh
            then cget sh
            else bget sh

    rangeLoadP threads = rangeLoadConvolutedP threads dUnrolledFill
    rangeLoadS = rangeLoadConvolutedS dUnrolledFill

    {-# INLINE index #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE rangeLoadS #-}


{-# INLINE rangeLoadConvolutedP #-}
rangeLoadConvolutedP
        threads
        blockFill
        arr@(Convoluted _ _ bget center cget) tarr
        start end =

    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks [center, loadRange]

        wr = write tarr
        
        threadWorks =
            if blockSize loadCenter <= 0

                then fork threads start end (S.fill bget wr)
                
                else
                    let centerWorks = fork threads cs ce (blockFill cget wr)

                        shavings = clipBlock loadRange loadCenter
                        borders = filter ((> 0) . blockSize) shavings
                        threadBorders = evenChunks borders threads

                        {-# INLINE threadWork #-}
                        threadWork centerWork borders = do
                            centerWork
                            P.mapM_
                                (\(bs, be) -> S.fill bget wr bs be)
                                borders

                    in P.zipWith threadWork centerWorks threadBorders
                                    
    in Par.parallel_ threadWorks


{-# INLINE rangeLoadConvolutedS #-}
rangeLoadConvolutedS
        blockFill
        arr@(Convoluted _ _ bget center cget) tarr
        start end =
            
    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks [center, loadRange]

        shavings = clipBlock loadRange loadCenter
        bounds = filter ((> 0) . blockSize) shavings

        wr = write tarr
    in do
        blockFill cget wr cs ce
        P.mapM_ (\(bs, be) -> S.fill bget wr bs be) bounds



instance Vector v e => UVecSource (SE CV) Dim2 CV v e where
    rangeLoadSlicesP threads = rangeLoadConvolutedSlicesP threads dUnrolledFill
    {-# INLINE rangeLoadSlicesP #-}


{-# INLINE rangeLoadConvolutedSlicesP #-}
rangeLoadConvolutedSlicesP threads blockFill separateArr tarr start end =
    let convolutedSlices = slices separateArr

        loadRange = (start, end)
        centers = V.map center convolutedSlices
        loadCenters = V.map (\c -> intersectBlocks [loadRange, c]) centers

        writes = V.map write (slices tarr)
        eachElem f = V.zipWith3 f convolutedSlices loadCenters writes


        {-# INLINE centerWorks #-}
        centerWorks sl (cs, ce) wr =
            fork threads cs ce (blockFill (centerGet sl) wr)

        allCenterWorks = V.foldl (++) [] (eachElem centerWorks)


        {-# INLINE borderWorks #-}
        borderWorks sl loadCenter wr =
            let shavings = clipBlock loadRange loadCenter
                borders = filter ((> 0) . blockSize) shavings
            in P.map (\(bs, be) -> S.fill (borderGet sl) wr bs be) borders

        allBorders = V.foldl (++) [] (eachElem borderWorks)

        threadCenterWorks = evenChunks allCenterWorks threads
        threadBorderWorks = evenChunks allBorders threads

        {-# INLINE threadWork #-}
        threadWork centerWorks borderWorks = do
            P.sequence_ centerWorks
            P.sequence_ borderWorks

        threadWorks = P.zipWith threadWork threadCenterWorks threadBorderWorks

    in Par.parallel_ threadWorks



instance (USource CV sh a, USource CV sh b, BlockShape sh) =>
        Fusion CV CV sh a b where
    fmapM f (Convoluted sh tch bget center cget) =
        Convoluted sh tch (f <=< bget) center (f <=< cget)

    fzipM fun arrs =
        let shapes = toList $ V.map extent arrs
            sh = intersect shapes

            centers = toList $ V.map center arrs
            ctr = intersectBlocks centers

            tch = V.mapM_ B.touch arrs

            bgets = V.map borderGet arrs
            {-# INLINE bget #-}
            bget sh = do
                v <- V.mapM ($ sh) bgets
                inspect v fun

            cgets = V.map centerGet arrs
            {-# INLINE cget #-}
            cget sh = do
                v <- V.mapM ($ sh) cgets
                inspect v fun

        in Convoluted sh tch bget ctr cget

    {-# INLINE fmapM #-}
    {-# INLINE fzipM #-}

instance (USource CV sh a, USource CV sh b, BlockShape sh) =>
        DefaultFusion CV CV sh a b



class (Arity n, Arity so, Arity eo) =>
        StencilOffsets n so eo | n -> so eo, so eo -> n where
    offsets :: n -> (so, eo)
    offsets _ = (undefined, undefined)
    {-# INLINE offsets #-}

instance StencilOffsets N1 Z Z
instance StencilOffsets N2 Z N1
instance (StencilOffsets (S n0) s0 e0) =>
        StencilOffsets (S (S (S n0))) (S s0) (S e0)

dim2ConvolveWithStaticStencil
    :: forall sx sox eox sy soy eoy r a b c d.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy,
        USource r Dim2 a)
    => sx -> sy                  -- Size of stencil
    -> VecList sy (VecList sx b) -- Stencil values
    -> (c -> a -> b -> c)        -- Generalized reduce function
    -> c                         -- Reduce zero
    -> UArray r Dim2 a           -- Source
    -> UArray CV Dim2 c
{-# INLINE dim2ConvolveWithStaticStencil #-}
dim2ConvolveWithStaticStencil _ _ stencil reduce z arr =
    let !startOffX = arity (undefined :: sox)
        !endOffX = arity (undefined :: eox)
        
        !startOffY = arity (undefined :: soy)
        !endOffY = arity (undefined :: eoy)

        {-# INLINE sget #-}
        sget get =
            \ (y, x) ->
                V.iifoldM
                    (-startOffY)
                    succ
                    (\acc iy xv ->
                        V.iifoldM
                            (-startOffX)
                            succ
                            (\acc ix b -> do
                                a <- get (y + iy, x + ix)
                                return $ reduce acc a b)
                            acc
                            xv)
                    z
                    stencil

        !sh@(shY, shX) = extent arr

        {-# INLINE slget #-}
        slget (y, x) =
            V.iifoldM
                (-startOffY)
                succ
                (\acc iy xv ->
                    let lbase = toIndex sh (y + iy, x)
                    in V.iifoldM
                        (-startOffX)
                        succ
                        (\acc ix b -> do
                            a <- linearIndex arr (lbase + ix)
                            return $ reduce acc a b)
                        acc
                        xv)
                z
                stencil

        {-# INLINE cget #-}
        cget = if shapeIndexingPreferred arr
                    then sget (index arr)
                    else slget
        

        {-# INLINE bget #-}
        bget = sget (\(y, x) ->
                        index arr (max 0 (min shY y), max 0 (min shX x)))

        tl = (startOffY, startOffX)
        br = (shY - endOffY, shX - endOffX)

    in Convoluted sh (B.touch arr) bget (tl, br) cget
