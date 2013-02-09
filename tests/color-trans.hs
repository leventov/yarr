{-# LANGUAGE BangPatterns, ScopedTypeVariables, MagicHash #-}

import Control.Monad
import Data.Word
import System.Environment
import GHC.Exts
import GHC.Prim

import Data.Yarr
import Data.Yarr.Shape as S
import Data.Yarr.IO.Image
import Data.Yarr.Benchmarking
import Data.Yarr.Utils.Primitive
import Data.Yarr.Utils.FixedVector


normalizeByte :: Word8 -> Float
normalizeByte w8 = (fromIntegral w8) / 255

normalizedToByte :: Float -> Word8
normalizedToByte f = fromIntegral (truncate (f * 255) :: Int)

main = do
    [cf, imageFile] <- getArgs

    anyImage <- readImage imageFile
    (floatRGBImage :: UArray F L Dim2 (VecList N3 Float)) <-
        compute (loadS fill) $
            mapElems normalizeByte $ readRGBVectors anyImage

    let contrastFactor = read cf
        !cc = 1.02 * (contrastFactor + 1) / (1.02 - contrastFactor)
        contrast comp = clampM 0.0 1.0 (cc * (comp - 0.5) + 0.5)
        delayedContrasted =
            mapElemsM ((return . normalizedToByte) <=< contrast) floatRGBImage

        ext = (extent floatRGBImage)
            
    (contrasted :: UArray F L Dim2 (VecList N3 Word8)) <- new ext
    bench "contrast" 10 ext $
        loadP S.fill caps delayedContrasted contrasted

    writeImage ("t-contrasted-" ++ imageFile) (RGB contrasted)


    -- Unfortunately, without this ↘ signature GHC doesn't inline the function
    let luminosity r g b = (0.21 :: Float) * r + 0.71 * g + 0.07 * b
        delayedLum = dmap normalizedToByte $ zipElems luminosity floatRGBImage

    (lum :: UArray F L Dim2 Word8) <- new ext
    bench "luminosity" 10 ext $
        loadP (S.unrolledFill n6 noTouch) caps delayedLum lum

    writeImage ("t-luminosity-" ++ imageFile) (Grey lum)