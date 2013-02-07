
module Data.Yarr.Utils.Parallel where

import Control.Monad
import GHC.Conc
import Control.Concurrent.MVar

parallel :: Int -> (Int -> IO a) -> IO [a]
{-# INLINE parallel #-}
parallel !threads makeWork = do
    rvars <- sequence $ replicate threads newEmptyMVar
    let {-# INLINE work #-}
        work t var = do
            r <- makeWork t
            putMVar var r
    zipWithM_ forkOn [0..] $ zipWith work [0..threads-1] rvars
    mapM takeMVar rvars

parallel_ :: Int -> (Int -> IO a) -> IO ()
{-# INLINE parallel_ #-}
parallel_ threads makeWork = parallel threads makeWork >> return ()