{-# LANGUAGE InstanceSigs #-}

module Data.Yarr.Shape (
    -- * Flow types hierarchy
    module Data.Yarr.WorkTypes,

    -- * Shape and BlockShape
    Block, Shape(..), BlockShape(..), MultiShape(..),

    -- * Shape instances
    Dim1, Dim2, Dim3,

    -- * Specialized flow
    dim2BlockFill,

    -- * Touch functions
    Touchable(..), noTouch,
) where

import Prelude as P hiding (foldl, foldr)
import GHC.Exts

import Control.DeepSeq

import Data.Yarr.WorkTypes

import Data.Yarr.Utils.FixedVector as V hiding (foldl, foldr)
import Data.Yarr.Utils.LowLevelFlow
import Data.Yarr.Utils.Primitive
import Data.Yarr.Utils.Split


-- | Mainly for internal use.
-- Abstracts top-left -- bottom-right pair of indices.
type Block sh = (sh, sh)

-- | Class for column-major, regular composite array indices.
class (Eq sh, Bounded sh, Show sh, NFData sh) => Shape sh where
    -- | @0@, @(0, 0)@, @(0, 0, 0)@
    zero :: sh
    -- | 'Dim1' @size@ is 'id', @size (3, 5) == 15@
    size :: sh -> Int
    inc :: sh -> sh
    -- | @(1, 2, 3) \`plus\` (0, 0, 1) == (1, 2, 4)@
    plus :: sh -> sh -> sh
    -- | @(1, 2) \`minus\` (1, 0) == (0, 2)@ 
    minus :: sh -> sh -> sh
    minus = flip offset
    -- | @offset == 'flip' 'minus'@
    offset :: sh -> sh -> sh

    -- | Converts linear, memory index of shaped array to shape index
    -- without bound checks.
    -- 
    -- @fromLinear (3, 4) 5 == (1, 1)@
    fromLinear
        :: sh  -- ^ Extent of array
        -> Int -- ^ Linear index
        -> sh  -- ^ Shape index

    -- | Opposite to 'fromLinear', converts composite array index
    -- to linear, \"memory\" index without bounds checks.
    --
    -- 'id' for 'Dim1' shapes.
    --
    -- @toLinear (5, 5) (3, 0) == 15@
    toLinear
        :: sh  -- ^ Extent of array
        -> sh  -- ^ Shape index
        -> Int -- ^ Linear index
    
    -- | Component-wise minimum, returns maximum legal index
    -- for all given array extents
    intersect
        :: (Arity n, n ~ S n0)
        => VecList n sh -- ^ Several array extents
        -> sh           -- ^ Maximum common shape index

    -- | Component-wise maximum, used in "Data.Yarr.Convolution" implementation.
    complement :: (Arity n, n ~ S n0) => VecList n sh -> sh

    intersectBlocks :: (Arity n, n ~ S n0) => VecList n (Block sh) -> Block sh
    intersectBlocks blocks =
        let ss = V.map fst blocks
            es = V.map snd blocks
        in (complement ss, intersect es)

    blockSize :: Block sh -> Int
    insideBlock :: Block sh -> sh -> Bool

    makeChunkRange :: Int -> sh -> sh -> (Int -> Block sh)

    -- | Standard left fold wothout unrolling.
    --
    -- This one and 5 following functions shouldn't be called directly,
    -- they are intented to be passed as first argument
    -- to 'Data.Yarr.Eval.Load' and functions from
    -- "Data.Yarr.Work" module.
    foldl :: Foldl sh a b

    unrolledFoldl
        :: forall a b uf. Arity uf
        => uf           -- ^ Unroll factor
        -> (a -> IO ()) -- ^ 'touch' or 'noTouch'
        -> Foldl sh a b -- ^ Result curried function
                        -- to be passed to working functions

    -- | Standard right folding function without unrolling.
    foldr :: Foldr sh a b

    unrolledFoldr
        :: forall a b uf. Arity uf
        => uf           -- ^ Unroll factor
        -> (a -> IO ()) -- ^ 'touch' or 'noTouch'
        -> Foldr sh a b -- ^ Result curried function
                        -- to be passed to working functions

    -- | Standard fill without unrolling.
    -- To avoid premature optimization just type @fill@
    -- each time you want to 'Data.Yarr.Eval.Load' array
    -- to manifest representation.
    fill :: Fill sh a

    unrolledFill
        :: forall a uf. Arity uf
        => uf           -- ^ Unroll factor
        -> (a -> IO ()) -- ^ 'touch' or 'noTouch'
        -> Fill sh a    -- ^ Result curried function
                        -- to by passed to loading functions

    {-# INLINE minus #-}
    {-# INLINE intersectBlocks #-}

-- | For internal use.
--
-- /TODO:/ implement for 'Dim3' and merge with 'Shape' class
class (Shape sh, Arity (BorderCount sh)) => BlockShape sh where
    type BorderCount sh
    clipBlock
        :: Block sh                            -- ^ Outer block
        -> Block sh                            -- ^ Inner block
        -> VecList (BorderCount sh) (Block sh) -- ^ Shavings


class (Shape sh, Shape lsh) => MultiShape sh lsh | sh -> lsh, lsh -> sh where
    lower :: sh -> lsh
    inner :: sh -> Int
    combine :: lsh -> Int -> sh


type Dim1 = Int

instance Shape Dim1 where
    zero = 0
    size = id
    inc = succ
    plus = (+)
    offset off i = i - off
    fromLinear _ i = i
    toLinear _ i = i
    intersect = V.minimum
    complement = V.maximum

    blockSize (s, e) = e - s
    insideBlock (s, e) i = i >= s && i < e

    makeChunkRange chunks start end =
        let {-# INLINE split #-}
            split = makeSplitIndex chunks start end
        in \ !c -> (split c, split (c + 1))

    fill get write = \ (I# start#) (I# end#) -> fill# get write start# end#
    unrolledFill uf tch =
        \get write ->
        \ (I# start#) (I# end#) -> unrolledFill# uf tch get write start# end#

    foldl reduce mz get =
        \ (I# start#) (I# end#) -> foldl# reduce mz get start# end#

    unrolledFoldl unrollFactor tch reduce mz get =
        \ (I# start#) (I# end#) ->
            unrolledFoldl# unrollFactor tch reduce mz get start# end#

    foldr reduce mz get =
        \ (I# start#) (I# end#) -> foldr# reduce mz get start# end#

    unrolledFoldr unrollFactor tch reduce mz get =
        \ (I# start#) (I# end#) ->
            unrolledFoldr# unrollFactor tch reduce mz get start# end# 

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE inc #-}
    {-# INLINE plus #-}
    {-# INLINE offset #-}
    {-# INLINE fromLinear #-}
    {-# INLINE toLinear #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE makeChunkRange #-}

    {-# INLINE fill #-}
    {-# INLINE unrolledFill #-}

    {-# INLINE foldl #-}
    {-# INLINE unrolledFoldl #-}
    {-# INLINE foldr #-}
    {-# INLINE unrolledFoldr #-}


instance BlockShape Dim1 where
    type BorderCount Dim1 = N2
    clipBlock outer@(os, oe) inner =
        let intersection@(is, ie) = intersectBlocks (vl_2 inner outer)
        in (vl_2 (os, is) (ie, oe))

    {-# INLINE clipBlock #-}




type Dim2 = (Int, Int)

instance Shape Dim2 where
    zero = (0, 0)
    size (h, w) = h * w
    inc (h, w) = (h, w + 1)
    plus (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)
    offset (offY, offX) (y, x) = (y - offY, x - offX)
    fromLinear (_, w) i = i `quotRem` w
    toLinear (_, w) (y, x) = y * w + x
    
    intersect shapes =
        let hs = V.map fst shapes
            ws = V.map snd shapes
        in (V.minimum hs, V.minimum ws)

    complement shapes =
        let hs = V.map fst shapes
            ws = V.map snd shapes
        in (V.maximum hs, V.maximum ws)

    blockSize ((sy, sx), (ey, ex)) = (ey - sy) * (ex - sx)

    insideBlock ((sy, sx), (ey, ex)) (iy, ix) =
        (iy >= sy && iy < ey) && (ix >= sx && ix < ex)

    makeChunkRange chunks (sy, sx) (ey, ex) =
        let {-# INLINE range #-}
            range = makeChunkRange chunks sy ey
        in \c -> let (csy, cey) = range c in ((csy, sx), (cey, ex))

    fill get write =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y | y >= ey   = return ()
                     | otherwise = do
                        fill (\x -> get (y, x))
                             (\x a -> write (y, x) a)
                             sx ex
                        go (y + 1)
            in go sy

    unrolledFill
        :: forall a uf. Arity uf
        => uf                 -- Unroll factor
        -> (a -> IO ())       -- Touch
        -> Fill Dim2 a
    unrolledFill unrollFactor tch =
        let !(I# uf#) = arity unrollFactor
        in \get write ->
            \ ((I# sy#), (I# sx#)) ((I# ey#), (I# ex#)) ->
                let limX# = ex# -# uf#
                    {-# INLINE goY# #-}
                    goY# y#
                        | y# >=# ey#   = return ()
                        | otherwise    = do
                            let y = I# y#
                                {-# INLINE goX# #-}
                                goX# x#
                                    | x# ># limX# =
                                        fill#
                                            (\x -> get (y, x))
                                            (\x a -> write (y, x) a)
                                            x# ex#
                                    | otherwise   = do
                                        let x = I# x#
                                            is :: VecList uf (Int, Int)
                                            is = V.generate (\i -> (y, i + x))
                                        as <- V.mapM get is
                                        V.mapM_ tch as
                                        V.zipWithM_ write is as
                                        goX# (x# +# uf#)
                            goX# sx#
                            goY# (y# +# 1#)
                in goY# sy#

    foldl reduce mz get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y >= ey   = return b
                    | otherwise = do
                        b' <- foldl (\b x a -> reduce b (y, x) a)
                                    (return b)
                                    (\x -> get (y, x))
                                    sx ex
                        go (y + 1) b'
            in mz >>= go sy

    unrolledFoldl unrollFactor tch reduce mz get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y >= ey   = return b
                    | otherwise = do
                        b' <- unrolledFoldl
                                unrollFactor tch
                                (\b x a -> reduce b (y, x) a)
                                (return b)
                                (\x -> get (y, x))
                                sx ex
                        go (y + 1) b'
            in mz >>= go sy


    foldr reduce mz get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y < sy    = return b
                    | otherwise = do
                        b' <- foldr (\x a b -> reduce (y, x) a b)
                                    (return b)
                                    (\x -> get (y, x))
                                    sx ex
                        go (y - 1) b'
            in mz >>= go (ey - 1)

    unrolledFoldr unrollFactor tch reduce mz get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y < sy    = return b
                    | otherwise = do
                        b' <- unrolledFoldr
                                unrollFactor tch
                                (\x a b -> reduce (y, x) a b)
                                (return b)
                                (\x -> get (y, x))
                                sx ex
                        go (y - 1) b'
            in mz >>= go (ey - 1)

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE inc #-}
    {-# INLINE plus #-}
    {-# INLINE offset #-}
    {-# INLINE fromLinear #-}
    {-# INLINE toLinear #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE makeChunkRange #-}

    {-# INLINE fill #-}
    {-# INLINE unrolledFill #-}

    {-# INLINE foldl #-}
    {-# INLINE unrolledFoldl #-}
    {-# INLINE foldr #-}
    {-# INLINE unrolledFoldr #-}


instance BlockShape Dim2 where
    type BorderCount Dim2 = N4
    clipBlock outer@((osy, osx), (oey, oex)) inner =
        let intersection@((isy, isx), (iey, iex)) =
                intersectBlocks (vl_2 inner outer)
        in (vl_4 ((osy, isx), (isy, oex))
                 ((isy, iex), (oey, oex))
                 ((iey, osx), (oey, iex))
                 ((osy, osx), (iey, isx)))

    {-# INLINE clipBlock #-}

instance MultiShape Dim2 Dim1 where
    lower = fst
    inner = snd
    combine = (,)
    {-# INLINE lower #-}
    {-# INLINE inner #-}
    {-# INLINE combine #-}

-- | 2D-unrolled filling to maximize profit from
-- \"Global value numbering\" LLVM optimization.
--
-- Example:
--
-- @blurred <- 'Data.Yarr.Eval.compute' ('Data.Yarr.Eval.loadS' (dim2BlockFill 'n1' 'n4' 'touch')) delayedBlurred@
dim2BlockFill
    :: forall a bsx bsy. (Arity bsx, Arity bsy)
    => bsx                  -- ^ Block size by x. Use 'n1' - 'n8' values.
    -> bsy                  -- ^ Block size by y
    -> (a -> IO ())         -- ^ 'touch' or 'noTouch'
    -> Fill Dim2 a          -- ^ Result curried function
                            --   to be passed to loading functions.
{-# INLINE dim2BlockFill #-}
dim2BlockFill blockSizeX blockSizeY tch =
    \get write ->
    \ ((I# sy#), sx@(I# sx#)) end@((I# ey#), ex@(I# ex#)) ->
        let !(I# bx#) = arity blockSizeX
            limX# = ex# -# bx#

            !(I# by#) = arity blockSizeY
            limY# = ey# -# by#

            {-# INLINE goY# #-}
            goY# y# | y# ># limY# = fill get write ((I# y#), sx) end
                   | otherwise    = do
                        let y = I# y#
                            ys :: VecList bsy Int
                            ys = V.generate (+ y)

                            {-# INLINE go# #-}
                            go# x#
                                | x# ># limX# =
                                    fill get write
                                         (y, (I# x#)) (I# (y# +# by#), ex)
                                | otherwise   = do
                                    let xs :: VecList bsx Int
                                        xs = V.generate (+ (I# x#))
                                        is = V.map (\y -> V.map (\x -> (y, x)) xs) ys

                                    as <- V.mapM (V.mapM get) is
                                    V.mapM_ (V.mapM_ tch) as
                                    V.zipWithM_ (V.zipWithM_ write) is as

                                    go# (x# +# bx#)
                        go# sx#
                        goY# (y# +# by#)

        in goY# sy#




type Dim3 = (Int, Int, Int)

instance Shape Dim3 where
    zero = (0, 0, 0)
    size (d, h, w) = d * h * w
    inc (d, h, w) = (d, h, w + 1)
    plus (z1, y1, x1) (z2, y2, x2) = (z1 + z2, y1 + y2, x1 + x2)
    offset (offZ, offY, offX) (z, y, x) = (z - offZ, y - offY, x - offX)
    fromLinear (_, h, w) i =
        let (i', x) = i `quotRem` w
            (z, y) = i' `quotRem` h
        in (z, y, x)

    toLinear (_, h, w) (z, y, x) = z * (h * w) + y * w + x

    intersect shapes =
        let ds = V.map (\(d, _, _) -> d) shapes
            hs = V.map (\(_, h, _) -> h) shapes
            ws = V.map (\(_, _, w) -> w) shapes
        in (V.minimum ds, V.minimum hs, V.minimum ws)

    complement shapes =
        let ds = V.map (\(d, _, _) -> d) shapes
            hs = V.map (\(_, h, _) -> h) shapes
            ws = V.map (\(_, _, w) -> w) shapes
        in (V.maximum ds, V.maximum hs, V.maximum ws)

    blockSize ((sz, sy, sx), (ez, ey, ex)) =
        (ez - sz) * (ey - sy) * (ex - sx)

    insideBlock ((sz, sy, sx), (ez, ey, ex)) (iz, iy, ix) =
        (iz >= sz && iz < ez) &&
        (iy >= sy && iy < ey) &&
        (ix >= sx && ix < ex)

    makeChunkRange chunks (sz, sy, sx) (ez, ey, ex) =
        let {-# INLINE range #-}
            range = makeChunkRange chunks sz ez
        in \c -> let (csz, cez) = range c
                 in ((csz, sy, sx), (cez, ey, ex))


    fill get write =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z | z >= ez   = return ()
                     | otherwise = do
                        fill
                             (\(y, x) -> get (z, y, x))
                             (\(y, x) a -> write (z, y, x) a)
                             (sy, sx) (ey, ex)
                        go (z + 1)
            in go sz

    unrolledFill unrollFactor tch =
        let !uf = arity unrollFactor
            {-# INLINE actualFill #-}
            actualFill _ get write =
                \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
                    let {-# INLINE go #-}
                        go z | z >= ez   = return ()
                             | otherwise = do
                                unrolledFill
                                    unrollFactor tch
                                    (\(y, x) -> get (z, y, x))
                                    (\(y, x) a -> write (z, y, x) a)
                                    (sy, sx) (ey, ex)
                                go (z + 1)
                    in go sz
        in actualFill uf

    foldl reduce mz get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z >= ez   = return b
                    | otherwise = do
                        b' <- foldl
                                (\b (y, x) a -> reduce b (z, y, x) a)
                                (return b)
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z + 1) b'
            in mz >>= go sz

    unrolledFoldl unrollFactor tch reduce mz get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z >= ez   = return b
                    | otherwise = do
                        b' <- unrolledFoldl
                                unrollFactor tch
                                (\b (y, x) a -> reduce b (z, y, x) a)
                                (return b)
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z + 1) b'
            in mz >>= go sz


    foldr reduce mz get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z < sz    = return b
                    | otherwise = do
                        b' <- foldr
                                (\(y, x) a b -> reduce (z, y, x) a b)
                                (return b)
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z - 1) b'
            in mz >>= go (ez - 1)

    unrolledFoldr unrollFactor tch reduce mz get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z < sz    = return b
                    | otherwise = do
                        b' <- unrolledFoldr
                                unrollFactor tch
                                (\(y, x) a b -> reduce (z, y, x) a b)
                                (return b)
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z - 1) b'
            in mz >>= go (ez - 1)

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE inc #-}
    {-# INLINE plus #-}
    {-# INLINE offset #-}
    {-# INLINE fromLinear #-}
    {-# INLINE toLinear #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE makeChunkRange #-}

    {-# INLINE fill #-}
    {-# INLINE unrolledFill #-}

    {-# INLINE foldl #-}
    {-# INLINE unrolledFoldl #-}
    {-# INLINE foldr #-}
    {-# INLINE unrolledFoldr #-}

instance MultiShape Dim3 Dim2 where
    lower (z, y, _) = (z, y)
    inner (_, _, x) = x
    combine (z, y) x = (z, y, x)
    {-# INLINE lower #-}
    {-# INLINE inner #-}
    {-# INLINE combine #-}