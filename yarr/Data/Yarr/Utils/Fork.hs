
module Data.Yarr.Utils.Fork where

import Prelude as P

import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V hiding (generate)
import Data.Yarr.Utils.Parallel as Par


fork :: Shape sh
     => Int
     -> sh -> sh
     -> (sh -> sh -> IO a)
     -> [IO a]
{-# INLINE fork #-}
fork threads start end rangeWork =
    generate threads $ makeFork threads start end rangeWork


forkEachSlice
    :: (Shape sh, Arity n, v ~ VecList n)
    => Int
    -> sh -> sh
    -> v (sh -> sh -> IO a)
    -> [IO (v a)]
{-# INLINE forkEachSlice #-}
forkEachSlice threads start end rangeWorks =
    let {-# INLINE etWork #-}
        etWork = makeFork threads start end
    in generate threads $
        \ !t -> V.sequence $ V.map (\work -> etWork work t) rangeWorks


forkSlicesOnce
    :: (Shape sh, Arity n)
    => Int
    -> sh -> sh
    -> VecList n (sh -> sh -> IO a)
    -> [IO [(Int, a)]]
{-# INLINE forkSlicesOnce #-}
forkSlicesOnce !threads start end rangeWorks =
    let !slices = V.length rangeWorks
        !allChunks = lcm threads slices
        !chunksPerSlice = allChunks `quot` slices
        !chunksPerThread = allChunks `quot` threads

        {-# INLINE range #-}
        range = makeChunkRange chunksPerSlice start end

        {-# INLINE threadWork #-}
        threadWork startSlice startPos !endSlice !endPos =
            let {-# INLINE elemWork #-}
                elemWork !currSlice !currPos results =
                    if (currSlice > endSlice) ||
                       (currSlice == endSlice && endPos == start)
                        then return $ reverse results
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

    in generate threads $
        \ !t ->
            let startChunk = t * chunksPerThread
                (startSlice, stChunkInSl) = startChunk `quotRem` chunksPerSlice
                (startPos, _) = range stChunkInSl

                endChunk = (t + 1) * chunksPerThread - 1
                (endSlice, endChunkInSl) = endChunk `quotRem` chunksPerSlice
                (_, endPos) = range endChunkInSl

            in threadWork startSlice startPos endSlice endPos


makeFork
    :: Shape sh
    => Int
    -> sh -> sh
    -> ((sh -> sh -> IO a) -> (Int -> IO a))
{-# INLINE makeFork #-}
makeFork chunks start end =
    let {-# INLINE chunkRange #-}
        chunkRange = makeChunkRange chunks start end
    in \rangeWork ->
            \c ->
                let (cs, ce) = chunkRange c
                in rangeWork cs ce


{-# INLINE generate #-}
generate n produce = P.map produce [0..n-1]