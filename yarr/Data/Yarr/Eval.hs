
module Data.Yarr.Eval (
    safeFill,
    safeCompute,
) where

import Data.Yarr.Base as B
import Data.Yarr.Shape as S
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive as P


safeFill
    :: (USource r l sh a, UTarget tr tl sh b)
    => (UArray r l sh a -> UArray tr tl sh b -> IO ())
    -> UArray tr tl sh b -> UArray r l sh a -> IO ()
{-# INLINE safeFill #-}
safeFill load tarr arr = do
    load arr tarr
    B.touch arr
    B.touch tarr


safeCompute
    :: (USource r l sh a, Manifest tr tl mtr mtl sh b)
    => (UArray r l sh a -> UArray mtr mtl sh b -> IO ())
    -> UArray r l sh a -> IO (UArray tr tl sh b)
{-# INLINE safeCompute #-}
safeCompute load arr = do
    marr <- new (extent arr)
    safeFill load marr arr
    freeze marr
