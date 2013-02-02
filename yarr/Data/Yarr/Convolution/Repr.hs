
module Data.Yarr.Convolution.Repr where

import Prelude as P
import Control.Monad

import Data.Yarr.Base as B
import Data.Yarr.Shape as S
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel
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



instance BlockShape sh => USource CV sh a where
    index (Convoluted _ _ bget center cget) sh =
        if insideBlock center sh
            then cget sh
            else bget sh

    rangeLoadP threads = rangeLoadConvolutedP threads S.fill
    rangeLoadS = rangeLoadConvolutedS S.fill

    {-# INLINE index #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE rangeLoadS #-}

rangeLoadConvolutedP
    :: (BlockShape sh, UTarget tr sh a)
    => Int
    -> ((sh -> IO a) ->
        (sh -> a -> IO ()) ->
        sh -> sh ->
        IO ())
    -> UArray CV sh a
    -> UArray tr sh a
    -> sh -> sh
    -> IO ()
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
                                    
    in parallel_ threadWorks


rangeLoadConvolutedS
    :: (BlockShape sh, UTarget tr sh a)
    => ((sh -> IO a) ->
        (sh -> a -> IO ()) ->
        sh -> sh ->
        IO ())
    -> UArray CV sh a
    -> UArray tr sh a
    -> sh -> sh
    -> IO ()
{-# INLINE rangeLoadConvolutedS #-}
rangeLoadConvolutedS
        blockFill
        arr@(Convoluted _ _ bget center cget) tarr
        start end = do
            
    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks [center, loadRange]

        shavings = clipBlock loadRange loadCenter
        bounds = filter ((> 0) . blockSize) shavings

        wr = write tarr

    blockFill cget wr cs ce
    P.mapM_ (\(bs, be) -> S.fill bget wr bs be) bounds



instance (BlockShape sh, Vector v e) => UVecSource (SE CV) sh CV v e where
    rangeLoadSlicesP threads = rangeLoadConvolutedSlicesP threads S.fill
    {-# INLINE rangeLoadSlicesP #-}


rangeLoadConvolutedSlicesP
    :: (BlockShape sh,
        Vector v a, UVecTarget tr sh tslr v2 a, Dim v ~ Dim v2)
    => Int
    -> ((sh -> IO a) ->
        (sh -> a -> IO ()) ->
        sh -> sh ->
        IO ())
    -> UArray (SE CV) sh (v a)
    -> UArray tr sh (v2 a)
    -> sh -> sh
    -> IO ()
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

    in parallel_ threadWorks



instance (BlockShape sh, USource CV sh a, USource CV sh b) =>
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

instance (BlockShape sh, USource CV sh a, USource CV sh b) =>
        DefaultFusion CV CV sh a b
