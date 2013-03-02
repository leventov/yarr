{-# LANGUAGE
    FlexibleContexts, ScopedTypeVariables, BangPatterns #-}

import Data.Word
import System.Environment

import Data.Yarr
import Data.Yarr.Walk
import Data.Yarr.Shape as S
import Data.Yarr.Repr.Foreign
import Data.Yarr.IO.Image
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Benchmarking


type FImage = UArray F L Dim2
type FSImage = UArray FS L Dim2

main = do
    [imageFile] <- getArgs
    anyImage <- readImage imageFile

    let image = readRGBVectors anyImage
        ext = extent image
    
    (hslImage :: FImage (VecTuple N3 Float)) <-
        time "w8 rgb to float hsl" ext $
            dComputeP $
                dmap (rgb2hsl . V.map normalizeByte) image

    let lightness = (slices hslImage) V.! 2

    hist <- time "lightness hist fold" ext (computeHist lightness)

    cdf <- cumulateHist hist

    time "lightness equalization" ext (equalizeInPlace cdf lightness)
    
    (equalized :: FImage (VecList N3 Word8)) <-
        time "float hsl to w8 rgb" ext $
            dComputeP $
                dmap (convert . V.map normalizedToByte . hsl2rgb)
                     hslImage

    writeImage ("t-eq-" ++ imageFile) (RGB equalized)


normalizeByte :: Word8 -> Float
{-# INLINE normalizeByte #-}
normalizeByte w8 = (fromIntegral w8) / 255

normalizedToByte :: Float -> Word8
{-# INLINE normalizedToByte #-}
normalizedToByte f = fromIntegral (truncate (f * 255) :: Int)


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


computeHist :: FSImage Float -> IO (UArray F L Dim1 Int)
computeHist pixelValues =
    walkP caps (mutate (S.unrolledFill n8 noTouch) incHist)
               (newEmpty 256) joinHists pixelValues
  where
    incHist :: UArray F L Dim1 Int -> Float -> IO ()
    incHist hist value = do
        let level = truncate (value * 255.99)
        c <- index hist level
        write hist level (c + 1)

    joinHists h1 h2 = do
        loadS S.fill (dzip2 (+) h1 h2) h1
        return h1


cumulateHist :: UArray F L Dim1 Int -> IO (UArray F L Dim1 Int)
cumulateHist hist = do
    cdf <- new (extent hist)
    let acc c i v = do
            let nc = c + v
            write cdf i nc
            return nc
    iwalk (S.foldl acc) (return 0) hist
    touchArray cdf
    return cdf

equalizeInPlace :: UArray F L Dim1 Int -> FSImage Float -> IO ()
equalizeInPlace cdf pixelValues = do
    minCdf <- index cdf 0
    let sz = size (extent pixelValues)
        d = fromIntegral (sz - minCdf)
        eq l = do
            c <- index cdf (truncate (l * 255))
            return $ (fromIntegral (c - minCdf)) / d
    loadP S.fill caps (dmapM eq pixelValues) pixelValues
    touchArray cdf
