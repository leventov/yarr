
module Data.Yarr.Repr.Convoluted where

import Prelude as P
import Control.Monad
import Data.List.Split
import GHC.Conc
import qualified Control.Concurrent.ParallelIO.Global as Global

import Data.Yarr.Base as B
import Data.Yarr.Repr.Delayed
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Touchable as T


data CV

instance Shape sh => URegular CV sh a where

    data UArray CV sh a =
        Convoluted
            !sh          -- Extent
            (IO ())      -- Touch
            (sh -> IO a) -- Border get
            !(sh, sh)    -- Center block
            (sh -> IO a) -- Center get

    extent (Convoluted sh _ _ _ _) = sh
    isReshaped _ = True
    touch (Convoluted _ tch _ _ _) = tch

    {-# INLINE extent #-}
    {-# INLINE isReshaped #-}
    {-# INLINE touch #-}

instance Shape sh => NFData (UArray CV sh a) where
    rnf (Convoluted sh tch bget center cget) =
        sh `deepseq` tch `seq` bget `seq` center `deepseq` cget `seq` ()
    {-# INLINE rnf #-}



instance Touchable a => USource CV Dim2 a where
    index (Convoluted _ _ bget center cget) sh =
        if insideBlock center sh
            then cget sh
            else bget sh

    rangeLoadP threads = dim2RangeLoadP threads (dim2BlockFill n2 n2 T.touch)
    rangeLoadS = dim2RangeLoadS (dim2BlockFill n2 n2 T.touch)

    {-# INLINE index #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE rangeLoadS #-}

{-# INLINE dim2RangeLoadP #-}
dim2RangeLoadP
        threads
        blockFill
        arr@(Convoluted _ _ bget center cget) tarr
        start end =

    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks [center, loadRange]

        wr = write tarr
        
        threadWorks =
            if blockSize loadCenter <= 0

                then fork threads start end(dUnrolledFill bget wr)
                
                else
                    let centerWorks = fork threads cs ce (blockFill cget wr)

                        shavings = clipBlock loadRange loadCenter
                        bounds = filter ((> 0) . blockSize) shavings
                        !boundsCount = P.length bounds
                        !boundsPerThread = (boundsCount `quot` threads) + 1

                        threadBounds = chunksOf boundsPerThread bounds

                        {-# INLINE threadWork #-}
                        threadWork centerWork bounds = do
                            centerWork
                            P.mapM_
                                (\(bs, be) -> dUnrolledFill bget wr bs be)
                                bounds

                    in P.zipWith threadWork centerWorks threadBounds
                                    
    in Global.parallel_ threadWorks


{-# INLINE dim2RangeLoadS #-}
dim2RangeLoadS blockFill arr@(Convoluted _ _ bget center cget) tarr start end =
    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks [center, loadRange]

        shavings = clipBlock loadRange loadCenter
        bounds = filter ((> 0) . blockSize) shavings

        wr = write tarr
    in do
        blockFill cget wr cs ce
        P.mapM_ (\(bs, be) -> dUnrolledFill bget wr bs be) bounds



instance (USource CV sh a, USource CV sh b, BlockShape sh) =>
        Fusion CV CV sh a b where
    fmapM f (Convoluted sh tch bget center cget) =
        Convoluted sh tch (f <=< bget) center (f <=< cget)

    fzipM fun arrs =
        let shapes = toList $ V.map extent arrs
            centers =
                toList $ V.map (\(Convoluted _ _ _ center _) -> center) arrs

            sh = intersect shapes
            center = intersectBlocks centers

            tch = V.mapM_ B.touch arrs

            bgets = V.map (\(Convoluted _ _ bget _ _) -> bget) arrs
            {-# INLINE bget #-}
            bget sh = do
                v <- V.mapM ($ sh) bgets
                inspect v fun

            cgets = V.map (\(Convoluted _ _ _ _ cget) -> cget) arrs
            {-# INLINE cget #-}
            cget sh = do
                v <- V.mapM ($ sh) cgets
                inspect v fun

        in Convoluted sh tch bget center cget

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

convolveWithStencil
    :: forall sx sox eox sy soy eoy r a b c d.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy,
        USource r Dim2 a, Touchable c)
    => sx -> sy                  -- Size of stencil
    -> VecList sy (VecList sx b) -- Stencil values
    -> (c -> a -> b -> c)        -- Generalized reduce function
    -> c                         -- Reduce zero
    -> UArray r Dim2 a           -- Source
    -> UArray CV Dim2 c
{-# INLINE convolveWithStencil #-}
convolveWithStencil _ _ stencil reduce z arr =
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
        cget = if isReshaped arr
                    then sget (index arr)
                    else slget
        

        {-# INLINE bget #-}
        bget = sget (\(y, x) ->
                        index arr (max 0 (min shY y), max 0 (min shX x)))

        tl = (startOffY, startOffX)
        br = (shY - endOffY, shX - endOffX)

    in Convoluted sh (B.touch arr) bget (tl, br) cget










