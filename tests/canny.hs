{-# LANGUAGE
    FlexibleContexts, ScopedTypeVariables, BangPatterns, MagicHash,
    QuasiQuotes #-}

import Prelude as P
import Control.Monad

import Data.Int
import Data.Word
import GHC.Exts
import GHC.Word

import System.Environment
import System.IO

import Data.Yarr as Y
import Data.Yarr.Shape as S
import Data.Yarr.Work
import Data.Yarr.Repr.Foreign
import Data.Yarr.Repr.Delayed
import Data.Yarr.Convolution as C
import Data.Yarr.IO.Image
import Data.Yarr.Benchmarking
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive as Pr


type FImage = UArray F L Dim2

{-# INLINE floatToWord8 #-}
floatToWord8 f = fromIntegral (truncate f :: Int)


main = do
    args <- getArgs
    case args of
        [imageFile] -> run 1 50 100 imageFile
        [repeats, threshLow, threshHigh, imageFile] ->
            run (read repeats) (read threshLow) (read threshHigh) imageFile
        _ -> hPutStrLn stderr
                ("canny [bench repeats, threshLow in [0,255], threshHigh in [0,255]], " ++
                 "imageFile")

run repeats threshLow threshHigh imageFile = do
    anyImage <- readImage imageFile

    image <-
        dComputeS $
            mapElems fromIntegral $ readRGBVectors anyImage

    edges <- newEmpty (extent image)
    
    bench "Total" repeats (extent image) $
        process threshLow threshHigh image edges

    writeImage ("t-canny-edges-" ++ imageFile) (Grey edges)


process :: Word8 -> Word8 -> FImage (VecList N3 Float) -> FImage Word8 -> IO ()
process threshLow threshHigh image resultEdges = do

    let ext = extent image

    let luminosity r g b = (0.21 :: Float) * r + 0.71 * g + 0.07 * b
        delayedLum = zipElems luminosity image

    greyImage <-
        time "luminosity" ext $
            compute (loadP (S.unrolledFill n4 noTouch) caps) $
                dmap floatToWord8 delayedLum

    blurred <- compute blur greyImage

    magOrient <- compute (gradientMagOrient threshLow) blurred

    edges <- supress threshHigh magOrient

    wildfire edges resultEdges


blur :: FImage Word8 -> FImage Float -> IO ()
blur image target = do

    let convolvedX :: UArray CV CVL Dim2 Word
        convolvedX =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1 4 6 4 1 |]
                (dmap fromIntegral image)

    (cX :: FImage Word16) <- new (extent image)
    time "blur conv. by X" (extent image) $
        loadP (S.unrolledFill n8 Pr.touch) caps
              (dmap fromIntegral convolvedX) cX

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

        blurred = dmap norm convolvedY

    time "blur conv. by Y" (extent image) $
        loadP (S.unrolledFill n6 Pr.touch) caps blurred target


noOrient = 0 :: Word8
posDiag  = 64 :: Word8
vert     = 128 :: Word8
negDiag  = 192 :: Word8
horiz    = 255 :: Word8

noEdge = 0 :: Word8
weak   = 128 :: Word8
strong = 255 :: Word8

gradientMagOrient
    :: Word8 -> FImage Float -> FImage (VecTuple N2 Word8) -> IO ()
gradientMagOrient !threshLow image target =
    time "magOrient" (extent image) $
        loadP S.fill caps delayedMagOrient target
  where
    delayedMagOrient = dzip2 magnitudeAndOrient gradientX gradientY

    gradientX =
        dConvolveLinearDim2WithStaticStencil
            [dim2St| -1  0  1
                     -2  0  2
                     -1  0  1 |]
            image

    gradientY =
        dConvolveLinearDim2WithStaticStencil
            [dim2St|  1   2   1
                      0   0   0
                     -1  -2  -1 |]
            image

    magnitudeAndOrient :: Float -> Float -> VecTuple N2 Word8
    magnitudeAndOrient gX gY =
        VT_2 (mag, if mag < threshLow then noOrient else orient gX gY)
      where
        mag = floatToWord8 $ sqrt (gX * gX + gY * gY)

        orient :: Float -> Float -> Word8
        orient gX gY
            | atan_1q < 0.33 = horiz
            | atan_1q > 0.66 = vert
            | otherwise      = posDiag + diagInv
          where
            rr = gY / gX
            (r, diagInv) =
                if rr < 0 then (negate rr, negDiag - posDiag) else (rr, 0)
            -- 2nd order Taylor series of atan,
            -- see http://stackoverflow.com/a/14101194/648955
            br = 0.596227 * r
            num = br + (r * r)
            atan_1q = num / (1.0 + br + num)


supress :: Word8 -> FImage (VecTuple N2 Word8) -> IO (FImage Word8)
supress !threshHigh magOrient = do
    let ext = extent magOrient
    supressed <- newEmpty ext

    let mags = V.head (slices magOrient)
        mg = index mags

        {-# INLINE isMax #-}
        isMax sh m m1 m2 = do
            mag1 <- m1
            when (m > mag1) $ do
                mag2 <- m2
                when (m > mag2) $ do
                     let e = if m < threshHigh then weak else strong
                     write supressed sh e
        
        {-# INLINE comparePts #-}
        comparePts sh@(y, x) (VT_2 (m, o))
            | o == noOrient= return ()
            | o == horiz   = isMax sh m (mg (y,     x - 1)) (mg (y,     x + 1))
            | o == vert    = isMax sh m (mg (y - 1, x    )) (mg (y + 1, x    ))
            | o == negDiag = isMax sh m (mg (y - 1, x - 1)) (mg (y + 1, x + 1))
            | o == posDiag = isMax sh m (mg (y - 1, x + 1)) (mg (y + 1, x - 1))
            | otherwise    = return ()

        shapedSupressed =
            ShapeDelayedTarget
                ext (touchArray supressed) (force supressed) comparePts

        {-# INLINE supressLoad #-}
        supressLoad arr tarr =
            rangeLoadP (S.dim2BlockFill n3 n3 Pr.touch) caps arr tarr
                       (1, 1) (ext `minus` (1, 1))

    time "supress" (ext `minus` (2, 2)) $
        supressLoad magOrient shapedSupressed
    return supressed


wildfire :: FImage Word8 -> FImage Word8 -> IO ()
wildfire edges target = do
    let ext@(h, w) = extent edges
        newStack :: IO (UArray F L Dim1 (VecTuple N2 Int16))
        newStack = new (h * w)

        {-# INLINE stackIndex #-}
        stackIndex stack i = do
            ix16 <- index stack i
            let (VT_2 ix) = V.map fromIntegral ix16
            return ix

        {-# INLINE stackWrite #-}
        stackWrite stack i ix =
            linearWrite stack i (V.map fromIntegral (VT_2 ix))

        {-# INLINE pushWeak #-}
        pushWeak stack ix top = do
            edge <- index edges ix
            if edge == noEdge
                then return top
                else do
                    stackWrite stack top ix
                    write edges ix noEdge
                    return (top + 1)

        {-# INLINE fire #-}
        fire stack top
            | top == 0  = return ()
            | otherwise = do
                let top' = top - 1
                ix@(y, x) <- stackIndex stack top'
                write target ix strong
                    >>  pushWeak stack (y - 1, x - 1) top'
                    >>= pushWeak stack (y - 1, x)
                    >>= pushWeak stack (y - 1, x + 1)

                    >>= pushWeak stack (y,     x - 1)
                    >>= pushWeak stack (y,     x + 1)

                    >>= pushWeak stack (y + 1, x - 1)
                    >>= pushWeak stack (y + 1, x)
                    >>= pushWeak stack (y + 1, x + 1)

                    >>= fire stack

        {-# INLINE tryFire #-}
        tryFire stack ix edge
            | edge /= strong = return ()
            | otherwise      = do
                stackWrite stack 0 ix
                write edges ix noEdge
                fire stack 1

    lastStack <-
        time "wildfire" (ext `minus` (2, 2)) $
            rangeWorkP caps
                (imutate (S.unrolledFill n4 noTouch) tryFire)
                newStack (\s1 s2 -> touchArray s1 >> return s2)
                edges (1, 1) (ext `minus` (1, 1))
    touchArray lastStack
    touchArray target
