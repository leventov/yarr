{-# LANGUAGE
    TypeFamilies, FlexibleContexts, FlexibleInstances,
    TypeSynonymInstances,
    RankNTypes, ScopedTypeVariables, InstanceSigs,
    MagicHash, BangPatterns #-}

module Data.Yarr.Utils.Fork where

import Prelude as P

import qualified Control.Concurrent.ParallelIO.Global as Global

import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V hiding (generate)


parallel_ :: Shape sh => Int -> (sh -> sh -> IO a) -> sh -> sh -> IO ()
{-# INLINE parallel_ #-}
parallel_ threads rangeLoad start end =
    Global.parallel_ $ fork threads start end rangeLoad

parallelElems_
    :: (Shape sh, Arity n)
    => Int
    -> VecList n (sh -> sh -> IO a)
    -> sh -> sh
    -> IO ()
{-# INLINE parallelElems_ #-}
parallelElems_ threads rangeLoads start end =
    Global.parallel_ $ forkElemsOnce threads start end rangeLoads

fork
    :: Shape sh
    => Int
    -> sh -> sh
    -> (sh -> sh -> IO a)
    -> [IO a]
{-# INLINE fork #-}
fork threads start end rangeWork =
    generate threads $ makeFork threads start end rangeWork


forkEachElem
    :: (Shape sh, Arity n, v ~ VecList n)
    => Int
    -> sh -> sh
    -> v (sh -> sh -> IO a)
    -> [IO (v a)]
{-# INLINE forkEachElem #-}
forkEachElem threads start end rangeWorks =
    let {-# INLINE etWork #-}
        etWork = makeFork threads start end
    in generate threads $
        \ !t -> V.sequence $ V.map (\work -> etWork work t) rangeWorks


forkElemsOnce
    :: (Shape sh, Arity n)
    => Int
    -> sh -> sh
    -> VecList n (sh -> sh -> IO a)
    -> [IO [(Int, a)]]
{-# INLINE forkElemsOnce #-}
forkElemsOnce !threads start end rangeWorks =
    let !elems = V.length rangeWorks
        !allChunks = lcm threads elems
        !chunksPerElem = allChunks `quot` elems
        !chunksPerThread = allChunks `quot` threads

        {-# INLINE range #-}
        range = makeChunkRange chunksPerElem start end

        {-# INLINE threadWork #-}
        threadWork startElem startPos !endElem !endPos =
            let {-# INLINE elemWork #-}
                elemWork !currElem !currPos results =
                    if (currElem > endElem) ||
                       (currElem == endElem && endPos == start)
                        then return $ reverse results
                        else
                            let endInE =
                                    if currElem == endElem then endPos else end
                            in do
                                r <- (rangeWorks V.! currElem) currPos endInE
                                elemWork
                                    (currElem + 1)
                                    start
                                    ((currElem, r):results)

            in elemWork startElem startPos []

    in generate threads $
        \ !t ->
            let startChunk = t * chunksPerThread
                (startElem, stChunkInE) = startChunk `quotRem` chunksPerElem
                (startPos, _) = range stChunkInE

                endChunk = (t + 1) * chunksPerThread - 1
                (endElem, endChunkInE) = endChunk `quotRem` chunksPerElem
                (_, endPos) = range endChunkInE

            in threadWork startElem startPos endElem endPos


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