{-# LANGUAGE
    FlexibleContexts, ScopedTypeVariables, BangPatterns #-}

import Data.Word
import System.Environment

import Data.Yarr
import Data.Yarr.Fold as F
import Data.Yarr.Shape as S
import Data.Yarr.Repr.Foreign
import Data.Yarr.IO.Image
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive
import Data.Yarr.Benchmarking

rgb2hsl :: VecTuple N3 Float -> VecTuple N3 Float
{-# INLINE rgb2hsl #-}
rgb2hsl rgb@(VT_3 (r, g, b))
    | mx == mn  = VT_3 (0, 0, mx)
    | otherwise = VT_3 (h, s, l)
    where
        mn = V.minimum rgb
        mx = V.maximum rgb
        d = mx - mn
        l = (mn + mx) / 2
        s | l <= 0.5  = d / (mn + mx)
          | otherwise = d / (2 - (mn + mx))

        h0 | r == mx   = (1/6) * (g - b) / d
           | g == mx   = (1/6) * (b - r) / d + 1/3
           | otherwise = (1/6) * (r - g) / d + 2/3
        h = if h0 < 0 then h0 + 1.0 else h0


hsl2rgb :: VecTuple N3 Float -> VecTuple N3 Float
{-# INLINE hsl2rgb #-}
hsl2rgb (VT_3 (h, s, l)) = V.map (comp . mod1) (VT_3 (h + 1/3, h, h - 1/3))
    where
        mod1 x = let (_, pf) = properFraction x
                 in if pf < 0 then pf + 1 else pf

        q | l < 0.5   = l * (1 + s)
          | otherwise = l + s - l * s

        p = 2 * l - q
        comp t | t < 1/6   = p + ((q - p) * 6 * t)
               | t < 1/2   = q
               | t < 2/3   = p + ((q - p) * 6 * (2/3 - t))
               | otherwise = p

type FImage = UArray F L Dim2

normalizeByte :: Word8 -> Float
{-# INLINE normalizeByte #-}
normalizeByte w8 = (fromIntegral w8) / 255

normalizedToByte :: Float -> Word8
{-# INLINE normalizedToByte #-}
normalizedToByte f = fromIntegral (truncate (f * 255) :: Int)

main = do
    [imageFile] <- getArgs
    anyImage <- readImage imageFile

    let image = readRGBVectors anyImage
        ext = extent image
    
    (hslImage :: FImage (VecTuple N3 Float)) <-
        time "w8 rgb to float hsl" ext $
            dComputeP $
                dmap (rgb2hsl . V.map normalizeByte) image



    let lum = (slices hslImage) V.! 2

        emptyHist :: IO (UArray F L Dim1 Int)
        emptyHist = newEmpty 256


        incHist hist i = do
            v <- index hist i
            write hist i (v + 1)
            return hist

        joinHist h1 h2 = do
            loadS (S.unrolledFill n4 noTouch) (dzip2 (+) h1 h2) h1
            return h1

        accHist hist l = incHist hist (truncate (l * 255))

    hist <- time "lum hist fold" ext $
        runFoldP caps
                 (reduceLeftM (S.unrolledFoldl n4 noTouch) accHist)
                 (emptyHist) joinHist lum

    let cdf = hist
        acc c i v = do
            let nc = c + v
            write cdf i nc
            return nc
    time "hist to cdf fold" (256 :: Int) $
        runIFold (S.unrolledFoldl n2 noTouch acc) (return 0) hist

    minCdf <- index cdf 0
    let sz = size ext
        d = fromIntegral (sz - minCdf)
        eq l = do
            c <- index cdf (truncate (l * 255))
            return $ (fromIntegral (c - minCdf)) / d

    time "lum equalization" ext $
        loadP S.fill caps (dmapM eq lum) lum

    (equalized :: FImage (VecList N3 Word8)) <-
        time "float hsl to w8 rgb" ext $
            dComputeP $
                dmap (convert . V.map normalizedToByte . hsl2rgb)
                     hslImage

    writeImage ("t-eq-" ++ imageFile) (RGB equalized)



