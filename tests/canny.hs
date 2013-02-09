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
import Data.Yarr.Repr.Foreign
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Checked
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
        [imageFile] -> run 50 100 imageFile
        [threshLow, threshHigh, imageFile] ->
            run (read threshLow) (read threshHigh) imageFile
        _ -> hPutStrLn stderr
                ("canny [threshLow in [0,255], threshHigh in [0,255]], " ++
                 "imageFile")

run threshLow threshHigh imageFile = do
    anyImage <- readImage imageFile

    image <-
        compute (loadS S.fill) $
            mapElems fromIntegral $ readRGBVectors anyImage

    edges <-
        time "Total" (extent image) $
            compute (process threshLow threshHigh) image

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

    let convolvedX :: UArray CV CV Dim2 Word
        convolvedX =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1 4 6 4 1 |]
                (dmap fromIntegral image)

    (cX :: FImage Word16) <- new (extent image)
    time "blur conv. by X" (extent image) $
        loadP (S.unrolledFill n8 Pr.touch) caps
              (dmap fromIntegral convolvedX) cX

    let convolvedY :: UArray CV CV Dim2 Word
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
        loadP (S.dim2BlockFill n1 n2 Pr.touch) caps blurred target


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
gradientMagOrient !threshLow image target = do

    let gradientX =
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
        {-# INLINE magnitudeAndOrient #-}
        magnitudeAndOrient !gX !gY =
            let mag = floatToWord8 $ sqrt (gX * gX + gY * gY)
                orient =
                    if mag < threshLow
                        then noOrient
                        else let !r = gY / gX
                                 !((F# r#), (W# diagInv#)) =
                                    if r < 0
                                        then (P.negate r, 128)
                                        else (r, 0)

                                 br# = 0.596227# `timesFloat#` r#
                                 num# = br# `plusFloat#` (r# `timesFloat#` r#)
                                 atan_1q# =
                                    num# `divideFloat#`
                                    (1.0# `plusFloat#` br# `plusFloat#` num#)

                             in if atan_1q# `leFloat#` 0.33#
                                    then horiz
                                    else if atan_1q# `geFloat#` 0.66#
                                            then vert
                                            else W8# (64## `plusWord#` diagInv#)
            in VT_2 (mag, orient)

        delayedMagOrient =
            dzip (Fun magnitudeAndOrient) (vl_2 gradientX gradientY)

    time "magOrient" (extent image) $
        loadP S.fill caps delayedMagOrient target


supress :: Word8 -> FImage (VecTuple N2 Word8) -> IO (FImage Word8)
supress !threshHigh magOrient = do

    let ext = extent magOrient
    supressed <- newEmpty ext

    let mags = V.head (slices magOrient)
        mg = index mags

        {-# INLINE isMax #-}
        isMax sh m m1 m2 = do
            mag1 <- m1
            if m < mag1
                then return ()
                else do
                    mag2 <- m2
                    if m < mag2
                        then return ()
                        else do
                            let e = if m < threshHigh
                                        then weak
                                        else strong
                            write supressed sh e
        
        {-# INLINE comparePts #-}
        comparePts sh@(y, x) (VT_2 (m, o))
            | o == noOrient= return ()
            | o == horiz   = isMax sh m (mg (y, x - 1)) (mg (y, x + 1))
            | o == vert    = isMax sh m (mg (y - 1, x)) (mg (y + 1, x))
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
        supressLoad (delayShaped magOrient) shapedSupressed

    return supressed


wildfire :: FImage Word8 -> FImage Word8 -> IO ()
wildfire edges target = do

    let ext = extent edges
    (stack :: FImage (VecTuple N2 Int16)) <- new ext

    let {-# INLINE stackIndex #-}
        stackIndex i = do
            ix16 <- linearIndex stack i
            let (VT_2 ix) = V.map fromIntegral ix16
            return ix

        {-# INLINE stackWrite #-}
        stackWrite i ix = linearWrite stack i (V.map fromIntegral (VT_2 ix))

        {-# INLINE pushWeak #-}
        pushWeak ix top = do
            edge <- index edges ix
            if edge == noEdge
                then return top
                else do
                    stackWrite top ix
                    write edges ix noEdge
                    return (top + 1)

        {-# INLINE fire #-}
        fire top
            | top == 0  = return ()
            | otherwise = do
                let !top' = top - 1
                ix@(!y, !x) <- stackIndex top'
                write target ix strong
                    >>  pushWeak (y - 1, x - 1) top'
                    >>= pushWeak (y - 1, x)
                    >>= pushWeak (y - 1, x + 1)

                    >>= pushWeak (y,     x - 1)
                    >>= pushWeak (y,     x + 1)

                    >>= pushWeak (y + 1, x - 1)
                    >>= pushWeak (y + 1, x)
                    >>= pushWeak (y + 1, x + 1)

                    >>= fire

        {-# INLINE pseudoWrite #-}
        pseudoWrite ix edge
            | edge /= strong = return ()
            | otherwise      = do
                stackWrite 0 ix
                write edges ix noEdge
                fire 1

    time "wildfire" (ext `minus` (2, 2)) $
        S.unrolledFill
            n4 noTouch
            (index edges) pseudoWrite
            (1, 1) (ext `minus` (1, 1))

    touchArray edges
    touchArray target
    touchArray stack
