
module Data.Yarr.Convolution.Eval () where

import Data.Yarr.Base
import Data.Yarr.Eval
import Data.Yarr.Shape as S
import Data.Yarr.Convolution.Repr
import Data.Yarr.Repr.Separate

import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel
import Data.Yarr.Utils.Split


instance Shape sh => PreferredWorkIndex CVL sh sh

instance (BlockShape sh, UTarget tr tl sh a) =>
        Load CV CVL tr tl sh a where
    type LoadIndex CVL tl sh = sh
    loadP fill threads arr tarr =
        cvLoadP fill threads arr tarr zero (entire arr tarr)
    loadS fill arr tarr = cvLoadS fill arr tarr zero (entire arr tarr)
    {-# INLINE loadP #-}
    {-# INLINE loadS #-}


instance (BlockShape sh, UTarget tr tl sh a) =>
        RangeLoad CV CVL tr tl sh a where
    rangeLoadP = cvLoadP
    rangeLoadS = cvLoadS
    {-# INLINE rangeLoadP #-}
    {-# INLINE rangeLoadS #-}

cvLoadP
    :: forall sh a tr tl. (BlockShape sh, UTarget tr tl sh a)
    => Fill sh a
    -> Threads
    -> UArray CV CVL sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
{-# INLINE cvLoadP #-}
cvLoadP fill threads arr@(Convoluted _ _ _ bget center cget) tarr start end = do
    force arr
    force tarr
    !ts <- threads

    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks (vl_2 center loadRange)

        {-# INLINE appFill #-}
        appFill = fill cget (write tarr)
        {-# INLINE centerWork #-}
        centerWork = makeFork ts cs ce appFill

        {-# INLINE borderFill #-}
        borderFill = S.fill bget (write tarr)

        !bordersCount = arity (undefined :: (BorderCount sh))
        {-# INLINE bordersSplit #-}
        bordersSplit = makeSplitIndex ts 0 bordersCount

        borders = clipBlock loadRange loadCenter

        {-# INLINE borderWork #-}
        borderWork !t =
            let !startBorder = bordersSplit t
                !endBorder = bordersSplit (t + 1)
                {-# INLINE go #-}
                go !b | b >= endBorder = return ()
                      | otherwise      = do
                            let (bs, be) = borders V.! b
                            borderFill bs be
                            go (b + 1)
            in go startBorder

        {-# INLINE threadWork #-}
        threadWork !t = do
            centerWork t
            borderWork t
                                    
    parallel_ ts threadWork

    touchArray arr
    touchArray tarr

cvLoadS
    :: (BlockShape sh, UTarget tr tl sh a)
    => Fill sh a
    -> UArray CV CVL sh a
    -> UArray tr tl sh a
    -> sh -> sh
    -> IO ()
{-# INLINE cvLoadS #-}
cvLoadS fill arr@(Convoluted _ _ _ bget center cget) tarr start end = do
    force arr
    force tarr

    let loadRange = (start, end)
        loadCenter@(cs, ce) = intersectBlocks (vl_2 center loadRange)
    fill cget (write tarr) cs ce

    let borders = clipBlock loadRange loadCenter
    V.mapM_ (\(bs, be) -> S.fill bget (write tarr) bs be) borders

    touchArray arr
    touchArray tarr


instance (BlockShape sh, Vector v e,
          UVecTarget tr tslr tl sh v2 e, Dim v ~ Dim v2,
          InlinableArity (Dim v)) =>
        VecLoad (SE CV) CV CVL tr tslr tl sh v v2 e where
    
    -- These functions aren't inlined propely with any first argument,
    -- different from Shape.fill (vanilla not unrolled fill),
    -- for an unknown reason

    loadSlicesP fill threads arr tarr =
        cvLoadSlicesP fill threads arr tarr zero (entire arr tarr)
    loadSlicesS fill arr tarr =
        cvLoadSlicesS fill arr tarr zero (entire arr tarr)
    {-# INLINE loadSlicesP #-}
    {-# INLINE loadSlicesS #-}

instance (BlockShape sh, Vector v e,
          UVecTarget tr tslr tl sh v2 e, Dim v ~ Dim v2,
          InlinableArity (Dim v)) =>
        RangeVecLoad (SE CV) CV CVL tr tslr tl sh v v2 e where
    rangeLoadSlicesP = cvLoadSlicesP
    rangeLoadSlicesS = cvLoadSlicesS
    {-# INLINE rangeLoadSlicesP #-}
    {-# INLINE rangeLoadSlicesS #-}


cvLoadSlicesP
    :: forall sh v e tr tslr tl v2.
       (BlockShape sh, UVecTarget tr tslr tl sh v2 e,
        Vector v e, Dim v ~ Dim v2, InlinableArity (Dim v))
    => Fill sh e
    -> Threads
    -> UArray (SE CV) CVL sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
{-# INLINE cvLoadSlicesP #-}
cvLoadSlicesP fill threads = \arr tarr start end -> do
    force arr
    force tarr
    !ts <- threads

    let loadRange = (start, end)
        sls = slices arr

        centers = V.map center sls

        loadCenters =
            V.inlinableMap (\c -> intersectBlocks (vl_2 c loadRange)) centers
        writes = V.inlinableMap write (slices tarr)
        borderGets = V.inlinableMap borderGet sls
        borderFills = V.inlinableZipWith S.fill borderGets writes

        centerGets = V.inlinableMap centerGet sls

        centerFills = V.inlinableZipWith fill centerGets writes

        {-# INLINE centerWork #-}
        centerWork = makeForkSlicesOnce ts loadCenters centerFills

        !slsCount = arity (undefined :: (Dim v))
        !bordersPerSlice = arity (undefined :: (BorderCount sh))
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

    touchArray arr
    touchArray tarr


cvLoadSlicesS
    :: (BlockShape sh, UVecTarget tr tslr tl sh v2 e,
        Vector v e, Dim v ~ Dim v2, InlinableArity (Dim v))
    => Fill sh e
    -> UArray (SE CV) CVL sh (v e)
    -> UArray tr tl sh (v2 e)
    -> sh -> sh
    -> IO ()
{-# INLINE cvLoadSlicesS #-}
cvLoadSlicesS fill arr tarr start end = do
    force arr
    force tarr
    
    let sls = slices arr
        borderGets = V.inlinableMap borderGet sls
        centers = V.map center sls

        centerGets = V.inlinableMap centerGet sls
        writes = V.inlinableMap write (slices tarr)
        centerFills = V.inlinableZipWith fill centerGets writes

        loadRange = (start, end)
        loadCenters = V.map (\c -> intersectBlocks (vl_2 c loadRange)) centers

    V.zipWithM_
        (\centerFill (cs, ce) -> centerFill cs ce)
        centerFills loadCenters

    let borders = V.map (clipBlock loadRange) loadCenters
        borderFills = V.inlinableZipWith S.fill borderGets writes
    V.zipWithM_
        (\bfill borders -> V.mapM_ (\(bs, be) -> bfill bs be) borders)
        borderFills borders

    touchArray arr
    touchArray tarr
