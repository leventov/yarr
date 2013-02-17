
module Data.Yarr.Convolution.StaticStencils (
    -- ** Dim1 stencils
    Dim1Stencil(..), dim1St,
    dConvolveDim1WithStaticStencil, convolveDim1WithStaticStencil,

    -- ** Dim2 stencils
    Dim2Stencil(..), dim2St, dim2OutClamp,
    dConvolveShDim2WithStaticStencil, convolveShDim2WithStaticStencil,
    dConvolveLinearDim2WithStaticStencil, convolveLinearDim2WithStaticStencil
) where

import Prelude as P
import Control.Monad
import Data.Char (isSpace)

import Language.Haskell.TH hiding (Arity)
import Language.Haskell.TH.Quote

import Data.Yarr.Base
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Convolution.Repr
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Primitive

-- | Generalized static 'Dim1' stencil.
data Dim1Stencil size a b c =
    Dim1Stencil {
        dim1StencilSize   :: size,
        dim1StencilValues :: (VecList size b),
        dim1StencilReduce :: (c -> a -> b -> IO c), -- ^ Generalized reduce function
        dim1StencilZero   :: c                      -- ^ Reduce zero
    }

-- | QuasiQuoter for producing typical numeric convolving 'Dim1' stencil,
-- which effectively skips unnecessary multiplications.
--
-- @[dim1St| 1 4 6 4 1 |]@
--
-- Produces
--
-- @
--'Dim1Stencil'
--    'n5'
--    ('VecList'
--       [\\ acc a -> return (acc + a),
--        \\ acc a -> (return $ (acc + (4 * a))),
--        \\ acc a -> (return $ (acc + (6 * a))),
--        \\ acc a -> (return $ (acc + (4 * a))),
--        \\ acc a -> return (acc + a)])
--    (\\ acc a reduce -> reduce acc a)
--    0
-- @
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


-- | Generalized static 'Dim2' stencil.
data Dim2Stencil sx sy a b c =
    Dim2Stencil {
        dim2StencilSizeX :: sx,
        dim2StencilSizeY :: sy,
        dim2StencilValues :: (VecList sy (VecList sx b)), -- ^ Stencil values, packed in nested vectors
        dim2StencilReduce :: (c -> a -> b -> IO c),       -- ^ Generalized reduce function
        dim2StencilZero :: c                              -- ^ Reduce zero
    }

-- | Most useful 'Dim2' stencil producer.
--
-- Typing
--
-- @
-- [dim2St| 1   2   1
--          0   0   0
--         -1  -2  -1 |]
-- @
--
-- Results to
--
-- @
-- 'Dim2Stencil'
--  'n3'
--  'n3'
--  ('VecList'
--     ['VecList'
--        [\\ acc a -> return (acc + a),
--         \\ acc a -> (return $ (acc + (2 * a))),
--         \\ acc a -> return (acc + a)],
--      'VecList'
--        [\\ acc _ -> return acc,
--         \\ acc _ -> return acc,
--         \\ acc _ -> return acc],
--      'VecList'
--        [\\ acc a -> return (acc - a),
--         \\ acc a -> (return $ (acc + (-2 * a))),
--         \\ acc a -> return (acc - a)]])
--  (\\ acc a reduce -> reducej acc a)
--  0
-- @
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


-- | Curried version of 'convolveDim1WithStaticStencil'
-- with border get clamping indices out of bounds to
-- @0@ or @('extent' source)@.
dConvolveDim1WithStaticStencil
    :: (StencilOffsets s so eo, USource r l Dim1 a)
    => Dim1Stencil s a b c  -- ^ Convolution stencil
    -> UArray r l Dim1 a    -- ^ Source array
    -> UArray CV CVL Dim1 c -- ^ Fused convolved result array
{-# INLINE dConvolveDim1WithStaticStencil #-}
dConvolveDim1WithStaticStencil =
    convolveDim1WithStaticStencil
        (\arr len ->
            let !maxI = len - 1
            in linearIndex arr <=< (clampM' 0 maxI))

-- | Convolves 'Dim1' array with static stencil.
convolveDim1WithStaticStencil
    :: forall r l s so eo a b c.
       (USource r l Dim1 a, StencilOffsets s so eo)
    => (UArray r l Dim1 a -> Dim1 -> Dim1 -> IO a)
                             -- ^ (Source array -> Extent of this array ->
                             --   Index (may be out of bounds) -> Result value):
                             --   Border index (to treat indices near to bounds)
    -> Dim1Stencil s a b c   -- ^ Convolution stencil
    -> UArray r l Dim1 a     -- ^ Source array
    -> UArray CV CVL Dim1 c  -- ^ Fused convolved result array
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
        len (touchArray arr) (force arr)
        (sget (borderIndex arr len))
        (startOff, len - endOff) (sget (linearIndex arr))


-- | Clamps 'Dim2' index out of bounds to the nearest one inside bounds.
dim2OutClamp
    :: USource r l Dim2 a
    => UArray r l Dim2 a
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

-- | Defined as
-- @dConvolveShDim2WithStaticStencil = 'convolveShDim2WithStaticStencil' 'dim2OutClamp'@
--
-- Example:
--
-- @
--let gradientX =
--        dConvolveLinearDim2WithStaticStencil
--            ['dim2St'| -1  0  1
--                     -2  0  2
--                     -1  0  1 |]
--            image
-- @
dConvolveShDim2WithStaticStencil
    :: (StencilOffsets sx sox eox, StencilOffsets sy soy eoy,
        USource r SH Dim2 a)
    => Dim2Stencil sx sy a b c  -- ^ Convolution stencil
    -> UArray r SH Dim2 a       -- ^ Source array
    -> UArray CV CVL Dim2 c     -- ^ Fused convolved result array
{-# INLINE dConvolveShDim2WithStaticStencil #-}
dConvolveShDim2WithStaticStencil =
    convolveShDim2WithStaticStencil dim2OutClamp

-- | Convolves 'Dim2' array with 'SH'aped load type with static stencil.
convolveShDim2WithStaticStencil
    :: forall r sx sox eox sy soy eoy a b c.
       (USource r SH Dim2 a,
        StencilOffsets sx sox eox, StencilOffsets sy soy eoy)
    => (UArray r SH Dim2 a -> Dim2 -> Dim2 -> IO a)
                               -- ^ (Source array -> Extent of this array ->
                               --   Index (may be out of bounds) -> Result value):
                               --   Border index (to treat indices near to bounds)
    -> Dim2Stencil sx sy a b c -- ^ Convolution stencil
    -> UArray r SH Dim2 a      -- ^ Source array
    -> UArray CV CVL Dim2 c    -- ^ Fused convolved result array
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

    in Convoluted
        sh (touchArray arr) (force arr)
        (sget (borderIndex arr sh)) (tl, br) (sget (index arr))

-- | Analog of 'dConvolveShDim2WithStaticStencil'
-- to convolve arrays with 'L'inear load index.
dConvolveLinearDim2WithStaticStencil
    :: (StencilOffsets sx sox eox, StencilOffsets sy soy eoy,
        USource r L Dim2 a)
    => Dim2Stencil sx sy a b c  -- ^ Convolution stencil
    -> UArray r L Dim2 a        -- ^ Source array
    -> UArray CV CVL Dim2 c     -- ^ Fused convolved result array
{-# INLINE dConvolveLinearDim2WithStaticStencil #-}
dConvolveLinearDim2WithStaticStencil =
    convolveLinearDim2WithStaticStencil dim2OutClamp

-- | Analog of 'convolveShDim2WithStaticStencil'
-- to convolve arrays with 'L'inear load index.
convolveLinearDim2WithStaticStencil
    :: forall r sx sox eox sy soy eoy a b c.
       (StencilOffsets sx sox eox, StencilOffsets sy soy eoy,
        USource r L Dim2 a)
    => (UArray r L Dim2 a -> Dim2 -> Dim2 -> IO a)
                               -- ^ (Source array -> Extent of this array ->
                               --   Index (may be out of bounds) -> Result value):
                               --   Border index (to treat indices near to bounds)
    -> Dim2Stencil sx sy a b c -- ^ Convolution stencil
    -> UArray r L Dim2 a       -- ^ Source array
    -> UArray CV CVL Dim2 c    -- ^ Fused convolved result array
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
                    let lbase = toLinear sh (y + iy, x)
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

    in Convoluted
        sh (touchArray arr) (force arr)
        (sget (borderIndex arr sh)) (tl, br) slget


class (Arity n, Arity so, Arity eo) =>
        StencilOffsets n so eo | n -> so eo, so eo -> n

instance StencilOffsets N1 Z Z
instance StencilOffsets N2 Z N1
instance (StencilOffsets (S n0) s0 e0) =>
        StencilOffsets (S (S (S n0))) (S s0) (S e0)