
module Data.Yarr.Convolution.StaticStencils where

import Prelude as P
import Control.Monad
import Data.Char (isSpace)

import Language.Haskell.TH hiding (Arity)
import Language.Haskell.TH.Quote

import Data.Yarr.Base as B
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Convolution.Repr
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive

data Dim1Stencil size a b c =
    Dim1Stencil
        size                  -- Stencil size
        (VecList size b)      -- Stencil values
        (c -> a -> b -> IO c) -- Generalized reduce function
        c                     -- Reduce zero

dim1St :: QuasiQuoter
dim1St = QuasiQuoter parseDim1Stencil undefined undefined undefined

parseDim1Stencil s =
    let values :: [Integer]
        values = P.map read (words s)
        size = P.length values    
        sizeType = P.foldr appT [t|Z|] (P.replicate size [t|S|])
        sz = [| undefined :: $sizeType |]
        vecList = [| VecList |] `appE` (listE (P.map justNonZero values))
    in [| Dim1Stencil $sz $vecList (\acc a reduce -> reduce acc a) 0 |]


data Dim2Stencil sx sy a b c =
    Dim2Stencil
        sx sy                       -- Stencil size by X and Y
        (VecList sy (VecList sx b)) -- Stencil values
        (c -> a -> b -> IO c)       -- Generalized reduce function
        c                           -- Reduce zero

dim2St :: QuasiQuoter
dim2St = QuasiQuoter parseDim2Stencil undefined undefined undefined

parseDim2Stencil s =
    let ls = filter (not . P.all isSpace) (lines s)
        values :: [[Integer]]
        values = P.map (P.map read . words) ls

        sizeX = P.length (P.head values)
        sizeTypeX = P.foldr appT [t|Z|] (P.replicate sizeX [t|S|])
        sx = [| undefined :: $sizeTypeX |]
        
        sizeY = P.length values
        sizeTypeY = P.foldr appT [t|Z|] (P.replicate sizeY [t|S|])
        sy = [| undefined :: $sizeTypeY |]

        vl = [| VecList |]
        innerLists =
            P.map (\vs -> vl `appE` (listE (P.map justNonZero vs))) values
        outerList = vl `appE` (listE innerLists)

    in [| Dim2Stencil $sx $sy $outerList (\acc a reduce -> reduce acc a) 0 |]


justNonZero :: Integer -> Q Exp
justNonZero v
    | v == 0    = [| \acc _ -> return acc |]
    | v == 1    = [| \acc a -> return (acc + a) |]
    | v == -1   = [| \acc a -> return (acc - a) |]
    | otherwise = [| \acc a -> return $ acc + $(litE (integerL v)) * a |]



dConvolveDim1WithStaticStencil
    :: (StencilOffsets s so eo, USource D l Dim1 a)
    => Dim1Stencil s a b c
    -> UArray D l Dim1 a
    -> UArray CV CV Dim1 c
{-# INLINE dConvolveDim1WithStaticStencil #-}
dConvolveDim1WithStaticStencil =
    convolveDim1WithStaticStencil
        (\arr len ->
            let !maxI = len - 1
            in linearIndex arr <=< (clampM' 0 maxI))


convolveDim1WithStaticStencil
    :: forall l s so eo a b c. (StencilOffsets s so eo, USource D l Dim1 a)
    => (UArray D l Dim1 a -> Dim1 -> Dim1 -> IO a)
    -> Dim1Stencil s a b c
    -> UArray D l Dim1 a
    -> UArray CV CV Dim1 c
{-# INLINE convolveDim1WithStaticStencil #-}
convolveDim1WithStaticStencil
        borderIndex (Dim1Stencil _ stencil reduce z) arr =

    let !startOff = arity (undefined :: so)
        !endOff = arity (undefined :: eo)

        {-# INLINE sget #-}
        sget get =
            \ix -> V.iifoldM
                    (-startOff)
                    succ
                    (\acc i b -> do
                        a <- get (ix + i)
                        reduce acc a b)
                    z
                    stencil

        !len = extent arr
    in Convoluted
            len (B.touch arr)
            (sget (borderIndex arr len))
            (startOff, len - endOff) (sget (linearIndex arr))



dim2OutClamp
    :: USource D l Dim2 a
    => UArray D l Dim2 a
    -> Dim2 -> Dim2
    -> IO a
{-# INLINE dim2OutClamp #-}
dim2OutClamp arr (shY, shX) =
    let !maxY = shY - 1
        !maxX = shX - 1
    in \(y, x) -> do
            y' <- clampM' 0 maxY y
            x' <- clampM' 0 maxX x
            index arr (y', x')


dConvolveShDim2WithStaticStencil
    :: (StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => Dim2Stencil sx sy a b c
    -> UArray D SH Dim2 a
    -> UArray CV CV Dim2 c
{-# INLINE dConvolveShDim2WithStaticStencil #-}
dConvolveShDim2WithStaticStencil =
    convolveShDim2WithStaticStencil dim2OutClamp

convolveShDim2WithStaticStencil
    :: forall sx sox eox sy soy eoy a b c.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => (UArray D SH Dim2 a -> Dim2 -> Dim2 -> IO a)
    -> Dim2Stencil sx sy a b c
    -> UArray D SH Dim2 a
    -> UArray CV CV Dim2 c
{-# INLINE convolveShDim2WithStaticStencil #-}
convolveShDim2WithStaticStencil
        borderIndex (Dim2Stencil _ _ stencil reduce z) arr =

    let !startOffX = arity (undefined :: sox)
        !endOffX = arity (undefined :: eox)
        
        !startOffY = arity (undefined :: soy)
        !endOffY = arity (undefined :: eoy)

        {-# INLINE sget #-}
        sget get =
            \ (y, x) ->
                V.iifoldM
                    (-startOffY)
                    succ
                    (\acc iy xv ->
                        V.iifoldM
                            (-startOffX)
                            succ
                            (\acc ix b -> do
                                a <- get (y + iy, x + ix)
                                reduce acc a b)
                            acc
                            xv)
                    z
                    stencil

        !sh@(shY, shX) = extent arr
        tl = (startOffY, startOffX)
        br = (shY - endOffY, shX - endOffX)

    in Convoluted sh (B.touch arr)
                  (sget (borderIndex arr sh)) (tl, br) (sget (index arr))


dConvolveLinearDim2WithStaticStencil
    :: (StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => Dim2Stencil sx sy a b c
    -> UArray D L Dim2 a
    -> UArray CV CV Dim2 c
{-# INLINE dConvolveLinearDim2WithStaticStencil #-}
dConvolveLinearDim2WithStaticStencil =
    convolveLinearDim2WithStaticStencil dim2OutClamp

convolveLinearDim2WithStaticStencil
    :: forall sx sox eox sy soy eoy a b c.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => (UArray D L Dim2 a -> Dim2 -> Dim2 -> IO a)
    -> Dim2Stencil sx sy a b c
    -> UArray D L Dim2 a
    -> UArray CV CV Dim2 c
{-# INLINE convolveLinearDim2WithStaticStencil #-}
convolveLinearDim2WithStaticStencil
        borderIndex (Dim2Stencil _ _ stencil reduce z) arr =

    let !startOffX = arity (undefined :: sox)
        !endOffX = arity (undefined :: eox)
        
        !startOffY = arity (undefined :: soy)
        !endOffY = arity (undefined :: eoy)

        {-# INLINE sget #-}
        sget get =
            \ (y, x) ->
                V.iifoldM
                    (-startOffY)
                    succ
                    (\acc iy xv ->
                        V.iifoldM
                            (-startOffX)
                            succ
                            (\acc ix b -> do
                                a <- get (y + iy, x + ix)
                                reduce acc a b)
                            acc
                            xv)
                    z
                    stencil

        !sh@(shY, shX) = extent arr

        {-# INLINE slget #-}
        slget !(!y, !x) =
            V.iifoldM
                (-startOffY)
                succ
                (\acc iy xv ->
                    let lbase = toIndex sh (y + iy, x)
                    in V.iifoldM
                        (-startOffX)
                        succ
                        (\acc ix b -> do
                            a <- linearIndex arr (lbase + ix)
                            reduce acc a b)
                        acc
                        xv)
                z
                stencil

        tl = (startOffY, startOffX)
        br = (shY - endOffY, shX - endOffX)

    in Convoluted sh (B.touch arr) (sget (borderIndex arr sh)) (tl, br) slget

 
stencilOffsets 1 = (0, 0)
stencilOffsets 2 = (0, 1)
stencilOffsets n
    | n <= 0    = error "Yarr! Stencil must be of positive size"
    | otherwise =
        let (s0, e0) = stencilOffsets (n - 2)
        in (s0 + 1, e0 + 1)

class (Arity n, Arity so, Arity eo) =>
        StencilOffsets n so eo | n -> so eo, so eo -> n

instance StencilOffsets N1 Z Z
instance StencilOffsets N2 Z N1
instance (StencilOffsets (S n0) s0 e0) =>
        StencilOffsets (S (S (S n0))) (S s0) (S e0)