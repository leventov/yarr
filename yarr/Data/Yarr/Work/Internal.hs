
module Data.Yarr.Work.Internal where

import Prelude as P
import Control.Monad as M
import Data.List (groupBy)
import Data.Function (on)

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Eval

import Data.Yarr.Utils.FixedVector as V hiding (toList)
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel

anyWork
    :: (USource r l sh a, WorkIndex sh i)
    => StatefulWork i a s
    -> IO s
    -> UArray r l sh a
    -> IO s
{-# INLINE anyWork #-}
anyWork fold mz arr = do
    force arr
    res <- fold mz (gindex arr) zero (gsize arr)
    touchArray arr
    return res


anyWorkP
    :: (USource r l sh a, WorkIndex sh i)
    => Threads
    -> StatefulWork i a s
    -> IO s
    -> (s -> s -> IO s)
    -> UArray r l sh a
    -> IO s
{-# INLINE anyWorkP #-}
anyWorkP threads fold mz join arr = do
    force arr
    ts <- threads
    (r:rs) <- parallel ts $
                makeFork ts zero (gsize arr) (fold mz (gindex arr))
    touchArray arr

    M.foldM join r rs


anyWorkOnSlicesSeparate
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => StatefulWork i e s
    -> IO s
    -> UArray r l sh (v e)
    -> IO (VecList (Dim v) s)
{-# INLINE anyWorkOnSlicesSeparate #-}
anyWorkOnSlicesSeparate fold mz arr = do
    force arr
    rs <- V.mapM (\sl -> anyWork fold mz sl) (slices arr)
    touchArray arr
    return rs


anyWorkOnSlicesSeparateP
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => Threads
    -> StatefulWork i e s
    -> IO s
    -> (s -> s -> IO s)
    -> UArray r l sh (v e)
    -> IO (VecList (Dim v) s)
{-# INLINE anyWorkOnSlicesSeparateP #-}
anyWorkOnSlicesSeparateP threads fold mz join arr = do
    force arr
    let sls = slices arr
    V.mapM force sls

    ts <- threads
    trs <- parallel ts $
            makeForkSlicesOnce
                ts
                (V.replicate (zero, gsize arr))
                (V.map (\sl -> fold mz (gindex sl)) sls)
    touchArray arr

    let rsBySlices = P.map (P.map snd) $ groupBy ((==) `on` fst) $ concat trs
    rs <- M.mapM (\(r:rs) -> M.foldM join r rs) rsBySlices
    return (VecList rs)