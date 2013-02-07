
module Data.Yarr.Convolution.Repr where

import Prelude as P
import Control.Monad

import Data.Yarr.Base as B
    hiding (rangeLoadP, rangeLoadS, rangeLoadSlicesP, rangeLoadSlicesS)
import Data.Yarr.Shape as S
import Foreign
import Data.Yarr.Repr.Foreign
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel
import Data.Yarr.Utils.Split


data CV

instance Shape sh => Regular CV CV sh a where

    data UArray CV CV sh a =
        Convoluted {
            getExtent :: !sh,
            getTouch :: (IO ()),
            borderGet :: (sh -> IO a),
            center :: !(sh, sh),
            centerGet :: (sh -> IO a)}

    extent = getExtent
    touch = getTouch

    {-# INLINE extent #-}
    {-# INLINE touch #-}

instance Shape sh => NFData (UArray CV CV sh a) where
    rnf (Convoluted sh tch bget center cget) =
        sh `deepseq` tch `seq` bget `seq` center `deepseq` cget `seq` ()
    {-# INLINE rnf #-}


instance Shape sh => USource CV CV sh a where
    index (Convoluted _ _ bget center cget) sh =
        if insideBlock center sh
            then cget sh
            else bget sh

    {-# INLINE index #-}


instance Shape sh => Fusion CV CV CV sh a b where
    fmapM f (Convoluted sh tch bget center cget) =
        Convoluted sh tch (f <=< bget) center (f <=< cget)

    fzipM fun arrs =
        let sh = intersect $ V.map extent arrs

            ctr = intersectBlocks $ V.map center arrs

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


instance Shape sh => DefaultFusion CV CV CV sh a b

instance (BlockShape sh, UTarget tr tl sh a) =>
        ULoad CV CV tr tl sh a where
    type LoadIndex CV tl sh = sh
    loadP = rangeLoadP
    loadS fill arr tarr = rangeLoadS fill arr tarr zero (entire arr tarr)
    {-# INLINE loadP #-}
    {-# INLINE loadS #-}

rangeLoadP
    :: (BlockShape sh, UTarget tr tl sh a)
    => Fill sh a
    -> Threads
    -> UArray CV CV sh a
    -> UArray tr tl sh a
    -> IO ()
rangeLoadP fill threads arr@(Convoluted _ _ bget center cget) tarr = do

    let loadRange@(start, end) = (zero, (entire arr tarr))
        loadCenter@(cs, ce) = intersectBlocks (vl_2 center loadRange)

    !ts <- threads
    let {-# INLINE centerWork #-}
        centerWork = makeFork ts cs ce (fill cget (write tarr))
        {-# INLINE threadWork #-}
        threadWork !t = do
            centerWork t
            if t == 0
                then let borders = clipBlock loadRange loadCenter
                     in V.mapM_ (\(bs, be) -> S.fill bget (write tarr) bs be)
                                borders
                else return ()
                                    
    parallel_ ts threadWork

rangeLoadS
    :: (BlockShape sh, UTarget tr tl sh a)
    => Fill sh a
    -> UArray CV CV sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
{-# INLINE rangeLoadS #-}
rangeLoadS fill arr tarr start end = do
    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks (vl_2 (center arr) loadRange)
    fill (centerGet arr) (write tarr) cs ce

    let borders = clipBlock loadRange loadCenter
    V.mapM_ (\(bs, be) -> S.fill (borderGet arr) (write tarr) bs be)
            borders



instance (BlockShape sh, Vector v e,
          UVecTarget tr tslr tl sh v2 e, Dim v ~ Dim v2) =>
        UVecLoad (SE CV) CV CV tr tslr tl sh v v2 e where
    
    -- These functions aren't inlined propely with any first argument,
    -- different from Shape.fill (vanilla not unrolled fill),
    -- for an unknown reason
    -- Блять, GHC, ну возьми и заинлайнь, там это даже руками можно сделать

    loadSlicesP fill threads arr tarr =
        rangeLoadSlicesP fill threads arr tarr zero (entire arr tarr)
    loadSlicesS fill arr tarr =
        rangeLoadSlicesS fill arr tarr zero (entire arr tarr)
    {-# INLINE loadSlicesP #-}
    {-# INLINE loadSlicesS #-}


rangeLoadSlicesP
    :: forall sh v e tr tslr tl v2.
       (BlockShape sh, UVecTarget tr tslr tl sh v2 e,
        Vector v e, Dim v ~ Dim v2)
    => Fill sh e
    -> Threads
    -> UArray (SE CV) CV sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
{-# INLINE rangeLoadSlicesP #-}
rangeLoadSlicesP fill threads arr tarr start end = do
    !ts <- threads
    let loadRange = (start, end)
        sls = slices arr

        centers = V.map center sls

        loadCenters = V.map (\c -> intersectBlocks (vl_2 c loadRange)) centers
        writes = V.map write (slices tarr)
        borderGets = V.map borderGet sls
        borderFills = V.zipWith S.fill borderGets writes

        centerGets = V.map centerGet sls
        centerFills = V.zipWith fill centerGets writes

        {-# INLINE centerWork #-}
        centerWork = makeForkSlicesOnce ts loadCenters centerFills

        !slsCount = arity (undefined :: (Dim v))
        !bordersPerSlice = arity (undefined :: (BC sh))
        !allBorders = slsCount * bordersPerSlice
        
        {-# INLINE bordersSplit #-}
        bordersSplit = makeSplitIndex ts 0 allBorders

        borders = V.map (clipBlock loadRange) loadCenters
        fillsAndBorders = V.zipWith (,) borderFills borders

        {-# INLINE bordersWork #-}
        bordersWork !t =
            let !startChunk = bordersSplit t
                !endChunk = (bordersSplit (t + 1)) - 1
                (!startSlice, !startBorder) =
                    startChunk `quotRem` bordersPerSlice
                (!endSlice, !endBorder) =
                    endChunk `quotRem` bordersPerSlice
                {-# INLINE go #-}
                go sl b | sl > endSlice = return ()
                        | otherwise     =
                            let e = if sl == endSlice
                                        then endBorder
                                        else (bordersPerSlice - 1)
                                (bfill, borders) = fillsAndBorders V.! sl
                            in do goSl bfill borders b e
                                  go (sl + 1) 0

                {-# INLINE goSl #-}
                goSl bfill borders c e
                    | c > e     = return ()
                    | otherwise =
                        let (bs, be) = borders V.! c
                        in bfill bs be >> goSl bfill borders (c + 1) e
            in go startSlice startBorder


        {-# INLINE threadWork #-}
        threadWork !t = do
            centerWork t
            bordersWork t

    parallel_ ts threadWork


rangeLoadSlicesS
    :: (BlockShape sh, UVecTarget tr tslr tl sh v2 e,
        Vector v e, Dim v ~ Dim v2)
    => Fill sh e
    -> UArray (SE CV) CV sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
{-# INLINE rangeLoadSlicesS #-}
rangeLoadSlicesS fill arr tarr start end = do
    let sls = slices arr
        borderGets = V.map borderGet sls
        centers = V.map center sls

        centerGets = V.map centerGet sls
        writes = V.map write (slices tarr)
        centerFills = V.zipWith fill centerGets writes

        loadRange = (start, end)
        loadCenters = V.map (\c -> intersectBlocks (vl_2 c loadRange)) centers

    V.zipWithM_
        (\centerFill (cs, ce) -> centerFill cs ce)
        centerFills loadCenters

    let borders = V.map (clipBlock loadRange) loadCenters
        borderFills = V.zipWith S.fill borderGets writes
    V.zipWithM_
        (\bfill borders -> V.mapM_ (\(bs, be) -> bfill bs be) borders)
        borderFills borders


    -- This version is not inlined propely for an unknown reason
    -- Какого хуя это не инлайнится??!

    --V.zipWithM_ (\sl tsl -> rangeLoadS fill sl tsl start end)
    --            (slices arr) (slices tarr)
