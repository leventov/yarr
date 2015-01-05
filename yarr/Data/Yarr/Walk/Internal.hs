
module Data.Yarr.Walk.Internal where

import Prelude as P
import Control.Monad as M
import Data.List (groupBy)
import Data.Function (on)

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Eval
import Data.Yarr.Repr.Delayed

import Data.Yarr.Utils.FixedVector as V hiding (toList, zero)
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel

anyReduceInner
    :: (USource r l sh a, MultiShape sh lsh, WorkIndex sh i)
    => StatefulWalk i a b
    -> (lsh -> IO b)
    -> UArray r l sh a
    -> UArray D SH lsh b
{-# INLINE anyReduceInner #-}
anyReduceInner fold getZ arr =
    ShapeDelayed (lower sh) (touchArray arr) (force arr) ix
  where
    sh = extent arr
    ix lsh =
        fold (getZ lsh) (gindex arr)
             (toWork (combine lsh 0))
             (toWork (combine (inc lsh) (inner sh)))

anyWalk
    :: (USource r l sh a, WorkIndex sh i)
    => StatefulWalk i a s
    -> IO s
    -> UArray r l sh a
    -> IO s
{-# INLINE anyWalk #-}
anyWalk fold mz arr = anyRangeWalk fold mz arr zero (gsize arr)

anyRangeWalk
    :: (USource r l sh a, WorkIndex sh i)
    => StatefulWalk i a s
    -> IO s
    -> UArray r l sh a
    -> i -> i
    -> IO s
{-# INLINE anyRangeWalk #-}
anyRangeWalk fold mz arr start end = do
    force arr
    res <- fold mz (gindex arr) start end
    touchArray arr
    return res


anyWalkP
    :: (USource r l sh a, WorkIndex sh i)
    => Threads
    -> StatefulWalk i a s
    -> IO s
    -> (s -> s -> IO s)
    -> UArray r l sh a
    -> IO s
{-# INLINE anyWalkP #-}
anyWalkP threads fold mz join arr =
    anyRangeWalkP threads fold mz join arr zero (gsize arr)

anyRangeWalkP
    :: (USource r l sh a, WorkIndex sh i)
    => Threads
    -> StatefulWalk i a s
    -> IO s
    -> (s -> s -> IO s)
    -> UArray r l sh a
    -> i -> i
    -> IO s
{-# INLINE anyRangeWalkP #-}
anyRangeWalkP threads fold mz join arr start end = do
    force arr
    ts <- threads
    (r:rs) <- parallel ts $
                makeFork ts start end (fold mz (gindex arr))
    touchArray arr

    M.foldM join r rs


anyWalkSlicesSeparate
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => StatefulWalk i e s
    -> IO s
    -> UArray r l sh (v e)
    -> IO (VecList (Dim v) s)
{-# INLINE anyWalkSlicesSeparate #-}
anyWalkSlicesSeparate fold mz arr =
    anyRangeWalkSlicesSeparate fold mz arr zero (gsize arr)

anyRangeWalkSlicesSeparate
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => StatefulWalk i e s
    -> IO s
    -> UArray r l sh (v e)
    -> i -> i
    -> IO (VecList (Dim v) s)
{-# INLINE anyRangeWalkSlicesSeparate #-}
anyRangeWalkSlicesSeparate fold mz arr start end = do
    force arr
    rs <- V.mapM (\sl -> anyRangeWalk fold mz sl start end) (slices arr)
    touchArray arr
    return rs

anyWalkSlicesSeparateP
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => Threads
    -> StatefulWalk i e s
    -> IO s
    -> (s -> s -> IO s)
    -> UArray r l sh (v e)
    -> IO (VecList (Dim v) s)
{-# INLINE anyWalkSlicesSeparateP #-}
anyWalkSlicesSeparateP threads fold mz join arr =
    anyRangeWalkSlicesSeparateP threads fold mz join arr zero (gsize arr)

anyRangeWalkSlicesSeparateP
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => Threads
    -> StatefulWalk i e s
    -> IO s
    -> (s -> s -> IO s)
    -> UArray r l sh (v e)
    -> i -> i
    -> IO (VecList (Dim v) s)
{-# INLINE anyRangeWalkSlicesSeparateP #-}
anyRangeWalkSlicesSeparateP threads fold mz join arr start end = do
    force arr
    let sls = slices arr
    V.mapM force sls

    ts <- threads
    trs <- parallel ts $
            makeForkSlicesOnce
                ts
                (V.replicate (start, end))
                (V.map (\sl -> fold mz (gindex sl)) sls)
    touchArray arr

    let rsBySlices = P.map (P.map snd) $ groupBy ((==) `on` fst) $ P.concat trs
    rs <- M.mapM (\(r:rs) -> M.foldM join r rs) rsBySlices
    return (fromList' rs)