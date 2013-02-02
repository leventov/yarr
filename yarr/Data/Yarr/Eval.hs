
module Data.Yarr.Eval (
    loadP, dLoadP, loadS,
    loadSlicesP, dLoadSlicesP, loadSlicesS,

    safeFill, fillP, fillS,
    fillSlicesP, fillSlicesS,
    
    safeCompute, computeP, computeS,
    computeSlicesP, computeSlicesS
) where

import GHC.Conc

import Data.Yarr.Base
import Data.Yarr.Shape


loadP :: (USource r sh a, UTarget tr sh a)
      => Int -> UArray r sh a -> UArray tr sh a -> IO ()
{-# INLINE loadP #-}
loadP threads arr tarr =
    genericLoad (rangeLoadP threads) (linearLoadP threads) arr tarr

dLoadP :: (USource r sh a, UTarget tr sh a)
       => UArray r sh a -> UArray tr sh a -> IO ()
{-# INLINE dLoadP #-}
dLoadP arr tarr = do
    threads <- getNumCapabilities
    loadP threads arr tarr
    
loadS :: (USource r sh a, UTarget tr sh a)
      => UArray r sh a -> UArray tr sh a -> IO ()
loadS arr tarr = genericLoad rangeLoadS linearLoadS arr tarr


loadSlicesP
    :: (UVecSource r sh slr v e, UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
    => Int -> UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
{-# INLINE loadSlicesP #-}
loadSlicesP threads arr tarr =
    genericLoad (rangeLoadSlicesP threads) (linearLoadSlicesP threads) arr tarr

dLoadSlicesP
    :: (UVecSource r sh slr v e, UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
    => UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
{-# INLINE dLoadSlicesP #-}
dLoadSlicesP arr tarr = do
    threads <- getNumCapabilities
    loadSlicesP threads arr tarr
    
loadSlicesS
    :: (UVecSource r sh slr v e, UVecTarget tr sh tslr v2 e, Dim v ~ Dim v2)
    => UArray r sh (v e) -> UArray tr sh (v2 e) -> IO ()
{-# INLINE loadSlicesS #-}
loadSlicesS arr tarr = genericLoad rangeLoadSlicesS linearLoadSlicesS arr tarr

genericLoad
    :: (USource r sh a, UTarget tr sh b)
    => (UArray r sh a -> UArray tr sh b -> sh -> sh -> IO ())
    -> (UArray r sh a -> UArray tr sh b -> IO ())
    -> UArray r sh a
    -> UArray tr sh b
    -> IO ()
{-# INLINE genericLoad #-}
genericLoad rangeLoad linearLoad arr tarr =
    if useRangeLoad arr tarr
        then rangeLoad
                arr tarr
                zero (intersect [extent arr, extent tarr])
        else linearLoad arr tarr

useRangeLoad
    :: (USource r sh a, UTarget tr sh b)
    => UArray r sh a -> UArray tr sh b -> Bool
{-# INLINE useRangeLoad #-}
useRangeLoad arr tarr =
    shapeIndexingPreferred arr ||
    shapeIndexingPreferred tarr ||
    (extent arr) /= (extent tarr)



safeFill
    :: (USource r sh a, UTarget tr sh b)
    => (UArray r sh a -> UArray tr sh b -> IO ())
    -> UArray tr sh b -> UArray r sh a -> IO ()
{-# INLINE safeFill #-}
safeFill load tarr arr = do
    load arr tarr
    touch arr
    touch tarr

fillP
    :: (USource r sh a, UTarget tr sh a)
    => UArray tr sh a -> UArray r sh a -> IO ()
{-# INLINE fillP #-}
fillP = safeFill dLoadP

fillS
    :: (USource r sh a, UTarget tr sh a)
    => UArray tr sh a -> UArray r sh a -> IO ()
{-# INLINE fillS #-}
fillS = safeFill loadS


fillSlicesP
    :: (UVecSource r sh slr v1 a, UVecTarget tr sh tslr v2 a,
        Dim v1 ~ Dim v2)
    => UArray tr sh (v2 a) -> UArray r sh (v1 a) -> IO ()
{-# INLINE fillSlicesP #-}
fillSlicesP = safeFill dLoadSlicesP

fillSlicesS
    :: (UVecSource r sh slr v1 a, UVecTarget tr sh tslr v2 a,
        Dim v1 ~ Dim v2)
    => UArray tr sh (v2 a) -> UArray r sh (v1 a) -> IO ()
{-# INLINE fillSlicesS #-}
fillSlicesS = safeFill loadSlicesS



safeCompute
    :: (USource r sh a, Manifest tr mtr sh b)
    => (UArray r sh a -> UArray mtr sh b -> IO ())
    -> UArray r sh a -> IO (UArray tr sh b)
{-# INLINE safeCompute #-}
safeCompute load arr = do
    marr <- new (extent arr)
    safeFill load marr arr
    freeze marr

computeP
    :: (USource r sh a, Manifest tr mtr sh a)
    => UArray r sh a -> IO (UArray tr sh a)
{-# INLINE computeP #-}
computeP = safeCompute dLoadP

computeS
    :: (USource r sh a, Manifest tr mtr sh a)
    => UArray r sh a -> IO (UArray tr sh a)
{-# INLINE computeS #-}
computeS = safeCompute loadS


computeSlicesP
    :: (UVecSource r sh slr v1 a,
        Manifest tr mtr sh (v2 a), UVecTarget mtr sh mtslr v2 a,
        Dim v1 ~ Dim v2)
    => (UArray r sh (v1 a)) -> IO (UArray tr sh (v2 a))
{-# INLINE computeSlicesP #-}
computeSlicesP = safeCompute dLoadSlicesP


computeSlicesS
    :: (UVecSource r sh slr v1 a,
        Manifest tr mtr sh (v2 a), UVecTarget mtr sh mtslr v2 a,
        Dim v1 ~ Dim v2)
    => (UArray r sh (v1 a)) -> IO (UArray tr sh (v2 a))
{-# INLINE computeSlicesS #-}
computeSlicesS = safeCompute loadSlicesS