
module Data.Yarr.Benchmarking where

import Prelude as P
import Control.Monad as M
import Text.Printf
import System.IO

import System.CPUTime.Rdtsc

import Data.Yarr.Base
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


timeSlices
    :: (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
        Dim v ~ Dim v2)
    => String
    -> Int
    -> sh
    -> (UArray slr l sh e -> UArray tslr tl sh e -> IO ())
    -> UArray r l sh (v e)
    -> UArray tr tl sh (v2 e)
    -> IO ()
{-# INLINE timeSlices #-}
timeSlices label repeats range load arr tarr = do
    V.izipWithM
        (\i sl tsl ->
            time (label ++ ": " ++ (show i)) repeats range load sl tsl)
        (slices arr) (slices tarr)
    return ()

time
    :: (USource r l sh a, UTarget tr tl sh b)
    => String
    -> Int
    -> sh
    -> (UArray r l sh a -> UArray tr tl sh b -> IO ())
    -> UArray r l sh a
    -> UArray tr tl sh b
    -> IO ()
{-# INLINE time #-}
time label repeats range load arr tarr = do
    (avTime, dev) <- benchLoad repeats load arr tarr
    let indices = fromIntegral (size range)
        avPerIndex = avTime / indices
        devPerIndex = dev / indices
    hPutStrLn stderr $
        printf
            "%s: %.1f ± %.1f tics per index (%d repeats)"
            label avPerIndex devPerIndex repeats


benchLoad
    :: (USource r l sh a, UTarget tr tl sh b)
    => Int
    -> (UArray r l sh a -> UArray tr tl sh b -> IO ())
    -> UArray r l sh a
    -> UArray tr tl sh b
    -> IO (Float, Float)
{-# INLINE benchLoad #-}
benchLoad repeats load arr tarr = do
    arr `deepseq` return ()
    tarr `deepseq` return ()
    let sample = do
            t1 <- rdtsc
            load arr tarr
            t2 <- rdtsc
            return $ fromIntegral (t2 - t1)

    times <- M.replicateM repeats sample
    let fRepeats = fromIntegral repeats
        avTime = (P.sum times) / fRepeats
        devs2 = P.map (\t -> (t - avTime) ^ 2) times
        stDev = sqrt . (/ fRepeats) . P.sum $ devs2
    return $ (avTime, stDev)