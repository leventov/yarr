
module Data.Yarr.Convolution.StaticStencils where

import Prelude as P
import Data.Char (isSpace)

import Language.Haskell.TH hiding (Arity)
import Language.Haskell.TH.Quote

import Data.Yarr.Base
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Convolution.Repr
import Data.Yarr.Utils.FixedVector as V


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
    let ls = filter (not . all isSpace) (lines s)
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
justNonZero v =
    if v == 0
        then [| \acc _ -> return acc |]
        else [| \acc a -> return $ acc + $(litE (integerL v)) * a |]



dConvolveDim1WithStaticStencil
    :: forall s so eo r a b c. (StencilOffsets s so eo)
    => Dim1Stencil s a b c
    -> UArray D Dim1 a
    -> UArray CV Dim1 c
{-# INLINE dConvolveDim1WithStaticStencil #-}
dConvolveDim1WithStaticStencil =
    convolveDim1WithStaticStencil
        (\arr len ->
            let !maxI = len - 1
            in \i -> linearIndex arr (max 0 (min maxI i)))


convolveDim1WithStaticStencil
    :: forall s so eo r a b c. (StencilOffsets s so eo)
    => (UArray D Dim1 a ->
        Dim1 ->
        Dim1 ->
        IO a)
    -> Dim1Stencil s a b c
    -> UArray D Dim1 a
    -> UArray CV Dim1 c
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
            len (touch arr)
            (sget (borderIndex arr len))
            (startOff, len - endOff) (sget (linearIndex arr))



dConvolveDim2WithStaticStencil
    :: forall sx sox eox sy soy eoy r a b c.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => Dim2Stencil sx sy a b c
    -> UArray D Dim2 a
    -> UArray CV Dim2 c
{-# INLINE dConvolveDim2WithStaticStencil #-}
dConvolveDim2WithStaticStencil =
    convolveDim2WithStaticStencil
        (\arr (shY, shX) ->
            let !maxY = shY - 1
                !maxX = shX - 1
            in \(y, x) -> index arr (max 0 (min maxY y), max 0 (min maxX x)))


convolveDim2WithStaticStencil
    :: forall sx sox eox sy soy eoy r a b c.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => (UArray D Dim2 a ->
        Dim2 ->
        Dim2 ->
        IO a)
    -> Dim2Stencil sx sy a b c
    -> UArray D Dim2 a
    -> UArray CV Dim2 c
{-# INLINE convolveDim2WithStaticStencil #-}
convolveDim2WithStaticStencil
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
        slget (y, x) =
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

        {-# INLINE cget #-}
        cget = if shapeIndexingPreferred arr
                    then sget (index arr)
                    else slget

        tl = (startOffY, startOffX)
        br = (shY - endOffY, shX - endOffX)

    in Convoluted sh (touch arr) (sget (borderIndex arr sh)) (tl, br) cget

 
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