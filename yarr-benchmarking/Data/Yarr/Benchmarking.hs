
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

instance URegular r sh a => URegular (B r) sh a where

    data UArray (B r) sh a =
        TimeIt {
            label :: String,
            repeats :: Int,
            dontTime :: UArray r sh a}
    
    extent (TimeIt _ _ arr) = extent arr
    isReshaped (TimeIt _ _ arr) = isReshaped arr
    touch (TimeIt _ _ arr) = touch arr
    
    {-# INLINE extent #-}
    {-# INLINE isReshaped #-}
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


instance UVecRegular r sh rsl v e => UVecRegular (B r) sh rsl v e where
    elems (TimeIt _ _ arr) = elems arr
    {-# INLINE elems #-}


instance UVecSource r sh slr v e => UVecSource (B r) sh slr v e where

    linearLoadElemsP threads (TimeIt label repeats arr) tarr =
        genericLoad
            (parallelLabel threads label) repeats
            (linearLoadElemsP threads)
            0 (size (extent arr))
            arr tarr

    rangeLoadElemsP threads (TimeIt label repeats arr) tarr start end =
        genericLoad
            (parallelLabel threads label) repeats
            (\arr tarr -> rangeLoadElemsP threads arr tarr start end)
            start end
            arr tarr

    linearLoadElemsS arr tarr =
        genericLoadElemsS arr tarr linearLoadS 0 (size (extent arr))

    rangeLoadElemsS arr tarr start end =
        genericLoadElemsS
            arr tarr (\sl tsl -> rangeLoadS sl tsl start end)
            start end

    {-# INLINE linearLoadElemsP #-}
    {-# INLINE rangeLoadElemsP #-}
    {-# INLINE linearLoadElemsS #-}
    {-# INLINE rangeLoadElemsS #-}


parallelLabel threads label =
    "Parallel in " ++ (show threads) ++ " threads: " ++ label


{-# INLINE genericLoadElemsS #-}
genericLoadElemsS (TimeIt label repeats arr) tarr load start end = do
    let slices = elems arr
        targetSlices = elems tarr
        loadElem i sl tsl =
            genericLoad
                (label ++ ": slice " ++ (show i)) repeats load
                start end sl tsl
    V.sequence $ V.izipWith loadElem slices targetSlices
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