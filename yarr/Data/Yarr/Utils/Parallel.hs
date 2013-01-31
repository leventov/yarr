
module Data.Yarr.Utils.Parallel where

import Control.Monad
import GHC.Conc
import Control.Concurrent.MVar

parallel :: [IO a] -> IO [a]
{-# INLINE parallel #-}
parallel actions = do
    let n = length actions
    results <- sequence $ replicate n newEmptyMVar
    let actions' = zipWith (\act var -> act >>= putMVar var) actions results
    zipWithM_ forkOn [0..n-1] actions'
    mapM takeMVar results

parallel_ :: [IO a] -> IO ()
{-# INLINE parallel_ #-}
parallel_ actions = parallel actions >> return ()