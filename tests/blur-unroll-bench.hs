{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, QuasiQuotes #-}

import Data.Word
import Text.Printf

import Data.Yarr
import Data.Yarr.Shape as S
import Data.Yarr.Convolution
import Data.Yarr.Utils.FixedVector
import Data.Yarr.Utils.Primitive as Pr
import Data.Yarr.Benchmarking

main = do
    
    let ext = (2560, 1600)
    (arr :: UArray F L Dim2 Word8) <- new ext

    let convolvedX :: UArray CV CVL Dim2 Int
        convolvedX =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1 4 6 4 1 |]
                (dmap fromIntegral arr)

        cXBenchRef :: IO ()
        {-# INLINE cXBenchRef #-}
        cXBenchRef = do
            let {-# INLINE load #-}
                load = loadP S.fill caps
            bench "by X: ref (no unroll)" 10 ext $
                do (r :: UArray F L Dim2 Word16) <-
                        compute load (dmap fromIntegral convolvedX)
                   return ()

        cXBench :: Arity a => a -> IO ()
        {-# INLINE cXBench #-}
        cXBench n = do
            let {-# INLINE load #-}
                load = loadP (S.unrolledFill n Pr.touch) caps
            bench (printf "by X: uf %d        " (arity n)) 10 ext $
                do (r :: UArray F L Dim2 Word16) <-
                        compute load (dmap fromIntegral convolvedX)
                   return ()

    cXBenchRef
    cXBench n1
    cXBench n2
    cXBench n3
    cXBench n4
    cXBench n5
    cXBench n6
    cXBench n7
    cXBench n8

    trace "\n"

    (cX :: UArray F L Dim2 Word16) <- new ext

    let convolvedY :: UArray CV CVL Dim2 Word
        convolvedY =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1
                         4
                         6
                         4
                         1 |]
                (dmap fromIntegral cX)

        norm :: Word -> Float
        {-# INLINE norm #-}
        norm w = (fromIntegral (fromIntegral w :: Int)) / 256

        cYBenchRef :: IO ()
        {-# INLINE cYBenchRef #-}
        cYBenchRef = do
            let {-# INLINE load #-}
                load = loadP S.fill caps
            bench "by Y: ref (no unroll)" 10 ext $
                do (r :: UArray F L Dim2 Float) <-
                        compute load (dmap norm convolvedY)
                   return ()

        cYBench :: Arity a => a -> IO ()
        {-# INLINE cYBench #-}
        cYBench n = do
            let {-# INLINE load #-}
                load = loadP (S.dim2BlockFill n1 n Pr.touch) caps
            bench (printf "by Y: uf %d        " (arity n)) 10 ext $
                do (r :: UArray F L Dim2 Float) <-
                        compute load (dmap norm convolvedY)
                   return ()

    cYBenchRef
    cYBench n1
    cYBench n2
    cYBench n3
    cYBench n4
    cYBench n5
    cYBench n6
    cYBench n7
    cYBench n8