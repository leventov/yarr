{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, BangPatterns #-}

import System.Environment
import Data.Word

import Data.Yarr
import Data.Yarr.Repr.Convoluted
import Data.Yarr.IO.Image
import Data.Yarr.Benchmarking
import Data.Yarr.Utils.FixedVector

blur :: UArray D Dim2 Int -> UArray CV Dim2 Float
{-# INLINE blur #-}
blur arr = 
    let convolved = 
            dim2ConvolveWithStaticStencil
                n5 n5
                (VecList [VecList [2, 4, 5, 4, 2],
                          VecList [4, 9,12, 9, 4],
                          VecList [5,12,15,12, 5],
                          VecList [4, 9,12, 9, 4],
                          VecList [2, 4, 5, 4, 2]])
                (\acc v i -> acc + i * v) 0
                arr
    in dmap ((/ 159) . fromIntegral) convolved

truncate' f = fromIntegral (truncate f :: Int)

main = do
    [imageFile] <- getArgs
    anyImage <- readImage imageFile

    (image :: UArray (SE F) Dim2 (VecList N3 Int)) <-
        computeS $ mapElems fromIntegral $ readRGBVectors anyImage

    let delayedImage = mapElems id image
        delayedBlurred = mapElems truncate' $ mapSlices blur delayedImage

    (blurred :: UArray F Dim2 (VecList N3 Word8)) <-
        computeSlicesS $ dTimeIt "sequential blur" delayedBlurred

    fillSlicesP blurred $ dTimeIt "parallel blur" delayedBlurred

    writeImage ("t-blurred-" ++ imageFile) (RGB blurred)
