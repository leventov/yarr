
module Data.Yarr.Utils.Parallel where

import Control.Monad
import GHC.Conc
import Control.Concurrent.MVar

parallel
    :: Int           -- ^ Number of threads to parallelize work on
    -> (Int -> IO a) -- ^ Per-thread work producer, passed
                     --   thread number @[0..threads-1]@
    -> IO [a]        -- ^ Results
{-# INLINE parallel #-}
parallel !threads makeWork = do
    rvars <- sequence $ replicate threads newEmptyMVar
    let {-# INLINE work #-}
        work t var = do
            r <- makeWork t
            putMVar var r
    zipWithM_ forkOn [0..] $ zipWith work [0..threads-1] rvars
    mapM takeMVar rvars

-- | Version of 'parallel' which discards results.
parallel_
    :: Int           -- ^ Number of threads to parallelize work on
    -> (Int -> IO a) -- ^ Per-thread work producer, passed
                     --   thread number @[0..threads-1]@
    -> IO ()
{-# INLINE parallel_ #-}
parallel_ threads makeWork = parallel threads makeWork >> return ()