{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Yarr.Utils.Fork where

import Prelude as P

import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V hiding (generate)
import Data.Yarr.Utils.Parallel as Par


makeForkEachSlice
    :: (Shape sh, Arity n, v ~ VecList n)
    => Int               -- ^ Number of threads to fork the work on
    -> sh                -- ^ Lower bound
    -> sh                -- ^ Upper bound
    -> v (Work sh a)     -- ^ Per-slice interval works
    -> (Int -> IO (v a)) -- ^ Producer of per-thread work,
                         -- which returns piece of result for each slice
{-# INLINE makeForkEachSlice #-}
makeForkEachSlice threads start end rangeWorks =
    let {-# INLINE etWork #-}
        etWork = makeFork threads start end
    in \ !t -> V.sequence $ V.map (\work -> etWork work t) rangeWorks


makeForkSlicesOnce
    :: (Shape sh, Arity n)
    => Int                    -- ^ Number of threads to fork the work on
    -> VecList n (sh, sh)     -- ^ (lower bound, upper bound) for each slice
    -> VecList n (Work sh a)  -- ^ Per-slice interval works
    -> (Int -> IO [(Int, a)]) -- ^ Producer of per-thread work,
                              -- which returns pieces of results: [(slice number, result)]
{-# INLINE makeForkSlicesOnce #-}
makeForkSlicesOnce !threads ranges rangeWorks =
    let !slices = V.length rangeWorks
        !allChunks = lcm threads slices
        !chunksPerSlice = allChunks `quot` slices
        !chunksPerThread = allChunks `quot` threads

        rangeMakers =
            V.map (\(s, e) -> makeChunkRange chunksPerSlice s e) ranges

        {-# INLINE threadWork #-}
        threadWork startSlice startPos !endSlice !endPos =
            let {-# INLINE elemWork #-}
                elemWork !currSlice !currPos results =
                    let (start, end) = ranges V.! currSlice
                    in if (currSlice > endSlice) ||
                          (currSlice == endSlice && endPos == start)
                        then return $ P.reverse results
                        else
                            let endInSl = if currSlice == endSlice
                                                then endPos
                                                else end
                            in do
                                r <- (rangeWorks V.! currSlice) currPos endInSl
                                elemWork
                                    (currSlice + 1)
                                    start
                                    ((currSlice, r):results)

            in elemWork startSlice startPos []

    in \ !t ->
            let startChunk = t * chunksPerThread
                (startSlice, stChunkInSl) = startChunk `quotRem` chunksPerSlice
                (startPos, _) = (rangeMakers V.! startSlice) stChunkInSl

                endChunk = (t + 1) * chunksPerThread - 1
                (endSlice, endChunkInSl) = endChunk `quotRem` chunksPerSlice
                (_, endPos) = (rangeMakers V.! endSlice) endChunkInSl

            in threadWork startSlice startPos endSlice endPos


makeFork
    :: Shape sh
    => Int            -- ^ Number of threads to fork the work on
    -> sh             -- ^ Lower bound
    -> sh             -- ^ Upper bound
    -> (Work sh a)    -- ^ Interval work
    -> (Int -> IO a)  -- ^ Producer of per-thread work
{-# INLINE makeFork #-}
makeFork chunks start end =
    let {-# INLINE chunkRange #-}
        chunkRange = makeChunkRange chunks start end
    in \rangeWork ->
            \c ->
                let (cs, ce) = chunkRange c
                in rangeWork cs ce
