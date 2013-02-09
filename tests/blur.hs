{-# LANGUAGE
    FlexibleContexts, ScopedTypeVariables, BangPatterns,
    QuasiQuotes #-}

import System.Environment
import Data.Word

import Data.Yarr
import Data.Yarr.Shape as S
import Data.Yarr.Convolution
import Data.Yarr.IO.Image
import Data.Yarr.Benchmarking
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive as P

blur :: UArray F L Dim2 Int -> UArray CV CV Dim2 Float
{-# INLINE blur #-}
blur arr =
    let convolved =
            convolveLinearDim2WithStaticStencil (\_ _ _ -> return 0)
                [dim2St| 2   4   5   4   2
                         4   9  12   9   4
                         5  12  15  12   5
                         4   9  12   9   4
                         2   4   5   4   2 |]
                arr
    in dmap ((/ 159) . fromIntegral) convolved

truncate' f = fromIntegral (truncate f :: Int)

main = do
    [imageFile] <- getArgs
    anyImage <- readImage imageFile

    (image :: UArray (SE F) L Dim2 (VecList N3 Int)) <-
        compute (loadS S.fill) $
            mapElems fromIntegral $ readRGBVectors anyImage

    let delayedBlurred = mapElems truncate' $ unsafeMapSlices blur image

    (blurred :: UArray F L Dim2 (VecList N3 Word8)) <- new (extent image)

    benchSlices "sequential blur" 10 (extent image)
                (loadS S.fill) delayedBlurred blurred

    bench "parallel blur" 10 (extent image) $
        -- It isn't inlined with S.unrolledFill and S.dim2BlockFill
        -- instead of S.fill, see comments in Data.Yarr.Convolution.Eval
        loadSlicesP S.fill caps delayedBlurred blurred

    writeImage ("t-blurred-" ++ imageFile) (RGB blurred)
