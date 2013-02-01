{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Data.Word
import System.Environment

import Data.Yarr
import Data.Yarr.IO.Image
import Data.Yarr.Benchmarking


clamp mn mx x = min mx (max mn x)

normalizeByte :: Word8 -> Float
normalizeByte w8 = (fromIntegral w8) / 255

normalizedToByte :: Float -> Word8
normalizedToByte f = fromIntegral (truncate (f * 255) :: Int)

main = do
    [cf, imageFile] <- getArgs

    anyImage <- readImage imageFile
    (floatRGBImage :: UArray F Dim2 (VecList N3 Float)) <-
        computeS $ mapElems normalizeByte $ readRGBVectors anyImage

    let contrastFactor = read cf
        !cc = 1.02 * (contrastFactor + 1) / (1.02 - contrastFactor)
        contrast comp = clamp 0.0 1.0 (cc * (comp - 0.5) + 0.5)
        delayedContrasted =
            mapElems (normalizedToByte . contrast) floatRGBImage
    (contrasted :: UArray F Dim2 (VecList N3 Word8)) <-
        computeP $ dTimeIt "contrast" delayedContrasted

    writeImage ("t-contrasted-" ++ imageFile) (RGB contrasted)

    -- Unfortunately, without this ↘ signature GHC doesn't inline the function
    let luminosity r g b = (0.21 :: Float) * r + 0.71 * g + 0.07 * b
        delayedLum = dmap normalizedToByte $ zipElems luminosity floatRGBImage
    (lum :: UArray F Dim2 Word8) <-
        computeP $ dTimeIt "luminosity" delayedLum

    writeImage ("t-luminosity-" ++ imageFile) (Grey lum)