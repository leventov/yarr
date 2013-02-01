
module Data.Yarr.Benchmarking where

import Prelude as P
import Control.Monad as M
import Text.Printf
import System.IO

import System.CPUTime.Rdtsc

import Data.Yarr.Base
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


data B r

instance Regular r sh a => Regular (B r) sh a where

    data UArray (B r) sh a =
        TimeIt {
            label :: String,
            repeats :: Int,
            dontTime :: UArray r sh a}
    
    extent (TimeIt _ _ arr) = extent arr
    shapeIndexingPreferred (TimeIt _ _ arr) = shapeIndexingPreferred arr
    touch (TimeIt _ _ arr) = touch arr
    
    {-# INLINE extent #-}
    {-# INLINE shapeIndexingPreferred #-}
    {-# INLINE touch #-}

dTimeIt :: String -> UArray r sh a -> UArray (B r) sh a
dTimeIt label arr = TimeIt label 10 arr

instance NFData (UArray r sh a) => NFData (UArray (B r) sh a) where
    rnf (TimeIt label repeats arr) =
        label `deepseq` repeats `seq` arr `deepseq` ()
    {-# INLINE rnf #-}


instance USource r sh a => USource (B r) sh a where
    index (TimeIt _ _ arr) = index arr
    linearIndex (TimeIt _ _ arr) = linearIndex arr

    linearLoadP threads (TimeIt label repeats arr) tarr =
        genericLoad
            (parallelLabel threads label) repeats
            (linearLoadP threads)
            0 (size (extent arr))
            arr tarr

    rangeLoadP threads (TimeIt label repeats arr) tarr start end =
        genericLoad
            (parallelLabel threads label) repeats
            (\arr tarr -> rangeLoadP threads arr tarr start end)
            start end
            arr tarr

    linearLoadS (TimeIt label repeats arr) tarr =
        genericLoad
            label repeats linearLoadS
            0 (size (extent arr))
            arr tarr
    
    rangeLoadS (TimeIt label repeats arr) tarr start end =
        genericLoad
            label repeats
            (\arr tarr -> rangeLoadS arr tarr start end)
            start end
            arr tarr
    
    {-# INLINE index #-}
    {-# INLINE linearIndex #-}
    {-# INLINE linearLoadP #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE linearLoadS #-}
    {-# INLINE rangeLoadS #-}


instance VecRegular r sh rsl v e => VecRegular (B r) sh rsl v e where
    slices (TimeIt _ _ arr) = slices arr
    {-# INLINE slices #-}


instance UVecSource r sh slr v e => UVecSource (B r) sh slr v e where

    linearLoadSlicesP threads (TimeIt label repeats arr) tarr =
        genericLoad
            (parallelLabel threads label) repeats
            (linearLoadSlicesP threads)
            0 (size (extent arr))
            arr tarr

    rangeLoadSlicesP threads (TimeIt label repeats arr) tarr start end =
        genericLoad
            (parallelLabel threads label) repeats
            (\arr tarr -> rangeLoadSlicesP threads arr tarr start end)
            start end
            arr tarr

    linearLoadSlicesS arr tarr =
        genericLoadSlicesS arr tarr linearLoadS 0 (size (extent arr))

    rangeLoadSlicesS arr tarr start end =
        genericLoadSlicesS
            arr tarr (\sl tsl -> rangeLoadS sl tsl start end)
            start end

    {-# INLINE linearLoadSlicesP #-}
    {-# INLINE rangeLoadSlicesP #-}
    {-# INLINE linearLoadSlicesS #-}
    {-# INLINE rangeLoadSlicesS #-}


parallelLabel threads label =
    "Parallel in " ++ (show threads) ++ " threads: " ++ label


{-# INLINE genericLoadSlicesS #-}
genericLoadSlicesS (TimeIt label repeats arr) tarr load start end = do
    let sourceSlices = slices arr
        targetSlices = slices tarr
        loadElem i sl tsl =
            genericLoad
                (label ++ ": slice " ++ (show i)) repeats load
                start end sl tsl
    V.sequence $ V.izipWith loadElem sourceSlices targetSlices
    return ()


{-# INLINE genericLoad #-}
genericLoad label repeats load from to arr tarr = do
    (avTime, dev) <- benchLoad repeats load arr tarr
    let indices = fromIntegral $ blockSize (from, to)
        avPerIndex = avTime / indices
        devPerIndex = dev / indices
    hPutStrLn stderr $
        printf
            "%s: %.1f ± %.1f tics per index (%d repeats)"
            label avPerIndex devPerIndex repeats


benchLoad
    :: (USource r sh a, UTarget tr sh b)
    => Int
    -> (UArray r sh a -> UArray tr sh b -> IO ())
    -> UArray r sh a
    -> UArray tr sh b
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