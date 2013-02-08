{-# LANGUAGE
    FlexibleContexts, ScopedTypeVariables, BangPatterns, MagicHash,
    QuasiQuotes #-}

import Prelude as P
import Control.Monad
import System.Environment
import System.IO
import Data.Word
import Data.Int
import GHC.Word
import GHC.Exts

import qualified Data.Array.Repa.IO.Timing as RT

import Data.Yarr as Y
import Data.Yarr.Shape as S
import Data.Yarr.Repr.Foreign
import Data.Yarr.Convolution as C
import Data.Yarr.IO.Image
import Data.Yarr.Benchmarking
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive as Pr

main = do
    args <- getArgs
    case args of
        [imageFile] -> run 1 50 100 imageFile
        [repeats, threshLow, threshHigh, imageFile] ->
            run (read repeats) (read threshLow) (read threshHigh) imageFile
        _ -> trace ("canny [bench repeats, threshLow, threshHigh] " ++
                    "imageFile")

type FImage = UArray F L Dim2

floatToWord8 f = fromIntegral (truncate f :: Int)

run repeats threshLow threshHigh imageFile = do
    anyImage <- readImage imageFile

    (image :: FImage (VecList N3 Float)) <-
        safeCompute (loadS (S.fill))
                    (mapElems fromIntegral $ readRGBVectors anyImage)

    (edges :: FImage Word8, tTotal)
        <- RT.time $ process repeats threshLow threshHigh image

    when (repeats == 1) $ trace (RT.prettyTime tTotal)

    writeImage ("t-canny-edges-" ++ imageFile) (Grey edges)

trace = hPutStrLn stderr

process :: Int -> Word8 -> Word8 -> FImage (VecList N3 Float) -> IO (FImage Word8)
process repeats threshLow threshHigh image = do
    let imageExt = extent image
        {-# INLINE time' #-}
        time' label = time label repeats imageExt


    let luminosity r g b = (0.21 :: Float) * r + 0.71 * g + 0.07 * b
        delayedLum = zipElems luminosity image

    greyImage
        <- safeCompute
            (time' "luminosity" (loadP (S.unrolledFill n4 noTouch) caps))
            (dmap floatToWord8 delayedLum)


    blurred <- new imageExt
    safeFill (time' "blur" blur) blurred greyImage


    magOrient <- gradientMagOrient repeats threshLow blurred

    let magExt = extent magOrient
        {-# INLINE time'' #-}
        time'' label = time label repeats magExt

    edges <- new magExt
    safeFill (time'' "supress" (supress threshHigh)) edges magOrient

    resultEdges <- newEmpty magExt
    stack <- new magExt
    safeFill (time'' "wildfire" (wildfire stack)) resultEdges edges

    return resultEdges



blur :: FImage Word8 -> FImage Float -> IO ()
{-# INLINE blur #-}
blur image target = do
    let convolved :: UArray CV CV Dim2 Word
        convolved =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 2   4   5   4   2
                         4   9  12   9   4
                         5  12  15  12   5
                         4   9  12   9   4
                         2   4   5   4   2 |]
                (Y.fmap fromIntegral image)
        blurred = dmap ((/ 159) . fromIntegral) convolved
    loadP S.fill caps blurred target


blur2 :: FImage Word8 -> FImage Float -> IO ()
{-# INLINE blur2 #-}
blur2 image target = do
    let convolvedX :: UArray CV CV Dim2 Word
        convolvedX =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1 4 6 4 1 |]
                (Y.fmap fromIntegral image)

    (cX :: FImage Word16)
        <- safeCompute
            (time "blur convolution by X" 1 (extent image)
                  (loadP (S.unrolledFill n4 Pr.touch) caps))
            (dmap fromIntegral convolvedX)


    let convolvedY :: UArray CV CV Dim2 Word
        convolvedY =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| 1
                         4
                         6
                         4
                         1 |]
                (Y.fmap fromIntegral cX)

        blurred = dmap ((/ 256) . fromIntegral) convolvedY

    (time "blur convolution by Y" 1 (extent image)
          (loadP (S.dim2BlockFill n1 n4 Pr.touch) caps))
        blurred target


noOrient = 0 :: Word8
posDiag  = 64 :: Word8
vert     = 128 :: Word8
negDiag  = 192 :: Word8
horiz    = 255 :: Word8

noEdge = 0 :: Word8
weak   = 128 :: Word8
strong = 255 :: Word8


gradientMagOrient
    :: Int -> Word8 -> FImage Float -> IO (FImage (VecTuple N2 Word8))
{-# INLINE gradientMagOrient #-}
gradientMagOrient repeats !threshLow image =
    let delayedImage = delay image
        gradientX =
            dConvolveLinearDim2WithStaticStencil
                [dim2St| -1  0  1
                         -2  0  2
                         -1  0  1 |]
                delayedImage

        gradientXCenter = justCenter gradientX

        gradientY =
            dConvolveLinearDim2WithStaticStencil
                [dim2St|  1   2   1
                          0   0   0
                         -1  -2  -1 |]
                delayedImage

        gradientYCenter = justCenter gradientY

        magnitudeAndOrient :: Float -> Float -> VecTuple N2 Word8
        magnitudeAndOrient !gX !gY =
            let mag = floatToWord8 $ sqrt (gX * gX + gY * gY)
                orient = if mag < threshLow
                                then noOrient
                                else let !d = atan2 gY gX
                                         !dRot = (d - (pi/8)) * (4/pi)
                                         !dNorm = if dRot < 0
                                                        then dRot + 8
                                                        else dRot

                                     in W8# (if dNorm >= 4
                                             then
                                              (if dNorm >= 6
                                               then
                                                (if dNorm >= 7
                                                 then 255##       -- 7
                                                 else 192##)      -- 6
                                               else
                                                (if dNorm >= 5
                                                  then 128##      -- 5
                                                  else 64##))     -- 4
                                             else
                                              (if dNorm >= 2
                                               then
                                                (if dNorm >= 3
                                                  then 255##      -- 3
                                                  else 192##)     -- 2
                                               else
                                                (if dNorm >= 1
                                                  then 128##      -- 1
                                                  else 64##)))    -- 0
            in VT_2 (mag, orient)

        delayedMagOrient =
            dzip (Fun magnitudeAndOrient) (vl_2 gradientX gradientY)

        sh = extent delayedMagOrient

    in safeCompute (time "magOrient" repeats sh (loadP S.fill caps))
                   delayedMagOrient



supress :: Word8 -> FImage (VecTuple N2 Word8) -> FImage Word8 -> IO ()
{-# INLINE supress #-}
supress !threshHigh magOrient target =

    let mags = V.head (slices magOrient)
        getMag = index mags
        
        {-# INLINE comparePts #-}
        comparePts sh@(!y, !x) = do
            (VT_2 (!m, !o)) <- index magOrient sh

            let {-# INLINE isMax #-}
                isMax m1 m2 = do
                    mag1 <- m1
                    if m < mag1
                        then return noEdge
                        else do
                            mag2 <- m2
                            return $
                                if m < mag2
                                    then noEdge
                                    else if m < threshHigh
                                            then weak
                                            else strong 

                {-# INLINE get #-}
                get _
                    | o == noOrient = return noEdge
                    | o == horiz    =
                        isMax (getMag (y, x - 1)) (getMag (y, x + 1))
                    | o == vert     =
                        isMax (getMag (y - 1, x)) (getMag (y + 1, x))
                    | o == negDiag  =
                        isMax (getMag (y - 1, x - 1)) (getMag (y + 1, x + 1))
                    | o == posDiag  =
                        isMax (getMag (y - 1, x + 1)) (getMag (y + 1, x - 1))
                    | otherwise     = return noEdge
            get ()

        sh = extent magOrient
        edges =
            Convoluted
                (sh) (Y.touch magOrient)
                (\_ -> return noEdge)
                ((1, 1), sh `minus` (1, 1))
                comparePts

    in loadP (S.dim2BlockFill n2 n2 Pr.touch) caps edges target



wildfire :: FImage (VecTuple N2 Int16) -> FImage Word8 -> FImage Word8 -> IO ()
{-# INLINE wildfire #-}
wildfire stack edges target =

    let stackIndex i = do
            ix16 <- linearIndex stack i
            let (VT_2 ix) = V.map fromIntegral ix16
            return ix
        stackWrite i ix = linearWrite stack i (V.map fromIntegral (VT_2 ix))

        {-# INLINE pushWeak #-}
        pushWeak !ix !top = do
            resE <- index target ix
            if resE == strong
                then return top
                else do
                    edge <- index edges ix
                    if edge == noEdge
                        then return top
                        else do
                            stackWrite top ix
                            write edges ix noEdge
                            return (top + 1)

        {-# INLINE fire #-}
        fire !top
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

    in S.fill (index edges) pseudoWrite (1, 1) ((extent edges) `minus` (1, 1))
