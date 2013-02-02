
module Data.Yarr.Convolution.Eval where

import GHC.Conc

import Data.Yarr.Base as B
import Data.Yarr.Shape
import Data.Yarr.Repr.Separate
import Data.Yarr.Convolution.Repr
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Touchable as T

dDim2BlockLoadConvolutedP
    :: (Arity bsx, Arity bsy, UTarget tr Dim2 a, Touchable a)
    => bsx -> bsy
    -> UArray CV Dim2 a
    -> UArray tr Dim2 a
    -> IO ()
{-# INLINE dDim2BlockLoadConvolutedP #-}
dDim2BlockLoadConvolutedP blockSizeX blockSizeY arr tarr = do
    threads <- getNumCapabilities
    dim2BlockLoadConvolutedP blockSizeX blockSizeY threads arr tarr

dim2BlockLoadConvolutedP
    :: (Arity bsx, Arity bsy, UTarget tr Dim2 a, Touchable a)
    => bsx -> bsy
    -> Int
    -> UArray CV Dim2 a
    -> UArray tr Dim2 a
    -> IO ()
{-# INLINE dim2BlockLoadConvolutedP #-}
dim2BlockLoadConvolutedP blockSizeX blockSizeY threads arr tarr =
    rangeLoadConvolutedP
        threads
        (dim2BlockFill blockSizeX blockSizeY T.touch)
        arr tarr
        zero (intersect [extent arr, extent tarr])


dim2BlockLoadConvolutedS
    :: (Arity bsx, Arity bsy, UTarget tr Dim2 a, Touchable a)
    => bsx -> bsy
    -> UArray CV Dim2 a
    -> UArray tr Dim2 a
    -> IO ()
{-# INLINE dim2BlockLoadConvolutedS #-}
dim2BlockLoadConvolutedS blockSizeX blockSizeY arr tarr =
    rangeLoadConvolutedS
        (dim2BlockFill blockSizeX blockSizeY T.touch)
        arr tarr
        zero (intersect [extent arr, extent tarr])



dDim2BlockLoadConvolutedSlicesP
    :: (Arity bsx, Arity bsy,
        Vector v a, UVecTarget tr Dim2 tslr v2 a, Dim v ~ Dim v2, Touchable a)
    => bsx -> bsy
    -> UArray (SE CV) Dim2 (v a)
    -> UArray tr Dim2 (v2 a)
    -> IO ()
{-# INLINE dDim2BlockLoadConvolutedSlicesP #-}
dDim2BlockLoadConvolutedSlicesP blockSizeX blockSizeY arr tarr = do
    threads <- getNumCapabilities
    dim2BlockLoadConvolutedSlicesP blockSizeX blockSizeY threads arr tarr

dim2BlockLoadConvolutedSlicesP
    :: (Arity bsx, Arity bsy,
        Vector v a, UVecTarget tr Dim2 tslr v2 a, Dim v ~ Dim v2, Touchable a)
    => bsx -> bsy
    -> Int
    -> UArray (SE CV) Dim2 (v a)
    -> UArray tr Dim2 (v2 a)
    -> IO ()
{-# INLINE dim2BlockLoadConvolutedSlicesP #-}
dim2BlockLoadConvolutedSlicesP blockSizeX blockSizeY threads arr tarr =
    rangeLoadConvolutedSlicesP
        threads
        (dim2BlockFill blockSizeX blockSizeY T.touch)
        arr tarr
        zero (intersect [extent arr, extent tarr])


dim2BlockLoadConvolutedSlicesS
    :: (Arity bsx, Arity bsy,
        Vector v a, UVecTarget tr Dim2 tslr v2 a, Dim v ~ Dim v2, Touchable a)
    => bsx -> bsy
    -> UArray (SE CV) Dim2 (v a)
    -> UArray tr Dim2 (v2 a)
    -> IO ()
{-# INLINE dim2BlockLoadConvolutedSlicesS #-}
dim2BlockLoadConvolutedSlicesS blockSizeX blockSizeY arr tarr =
    V.zipWithM_
        (\sl tsl -> dim2BlockLoadConvolutedS blockSizeX blockSizeY sl tsl)
        (slices arr) (slices tarr)