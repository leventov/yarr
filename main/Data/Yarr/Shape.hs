{-# LANGUAGE
    TypeFamilies, FlexibleContexts, FlexibleInstances,
    TypeSynonymInstances,
    RankNTypes, ScopedTypeVariables, InstanceSigs,
    MagicHash, BangPatterns #-}

module Data.Yarr.Shape where

import Prelude as P
import GHC.Exts

import Control.DeepSeq

import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Touchable


-- Can't reuse Dim from fixed-vector,
-- because it's parameter must be of kind * -> *
type family Rank sh

class (Eq sh, Show sh, NFData sh, Arity (Rank sh)) => Shape sh where
    zero :: sh
    size :: sh -> Int
    fromIndex :: sh -> Int -> sh
    toIndex :: sh -> sh -> Int
    intersect :: [sh] -> sh
    complement :: [sh] -> sh
    makeChunkRange :: Int -> sh -> sh -> (Int -> (sh, sh))

    fill
        :: (sh -> IO a)       -- get
        -> (sh -> a -> IO ()) -- write
        -> sh -> sh           -- start, end
        -> IO ()
    fill = unrolledFill n1 noTouch

    dUnrolledFill
        :: (sh -> IO a)       -- get
        -> (sh -> a -> IO ()) -- write
        -> sh -> sh           -- start, end
        -> IO ()
    dUnrolledFill = unrolledFill n4 noTouch

    unrolledFill
        :: forall a uf. Arity uf
        => uf                 -- unroll factor
        -> (a -> IO ())       -- touch
        -> (sh -> IO a)       -- get
        -> (sh -> a -> IO ()) -- write
        -> sh -> sh           -- start, end
        -> IO ()

    {-# INLINE fill #-}
    {-# INLINE dUnrolledFill #-}


class Shape sh => BlockShape sh where
    blockSize :: (sh, sh) -> Int

    insideBlock :: (sh, sh) -> sh -> Bool

    intersectBlocks :: [(sh, sh)] -> (sh, sh)
    intersectBlocks blocks =
        let (ss, es) = unzip blocks
        in (complement ss, intersect es)
    
    clipBlock :: (sh, sh) -> (sh, sh) -> [(sh, sh)]

    {-# INLINE intersectBlocks #-}


makeSplitIndex
    :: Int
    -> Int -> Int
    -> (Int -> Int)
{-# INLINE makeSplitIndex #-}
makeSplitIndex chunks start end =
    let !len = end - start
    in if len < chunks
            then \c -> start + (max c len)
            else let (chunkLen, chunkLeftover) = len `quotRem` chunks
                 in \c -> if c < chunkLeftover
                        then start + c * (chunkLen + 1)
                        else start + c * chunkLen + chunkLeftover



type Dim1 = Int
type instance Rank Dim1 = N1


instance Shape Dim1 where
    zero = 0
    size = id
    fromIndex _ i = i
    toIndex _ i = i
    intersect = P.minimum
    complement = P.maximum

    makeChunkRange chunks start end =
        let {-# INLINE split #-}
            split = makeSplitIndex chunks start end
        in \ !c -> (split c, split (c + 1))

    unrolledFill
        :: forall a uf. Arity uf
        => uf
        -> (a -> IO ()) -> (Dim1 -> IO a)
        -> (Dim1 -> a -> IO ())
        -> (Dim1 -> Dim1 -> IO ())
    unrolledFill tch get write bs =
        \ (I# start#) (I# end#) -> unrolledFill# tch get write bs start# end#

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE fromIndex #-}
    {-# INLINE toIndex #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE makeChunkRange #-}
    {-# INLINE unrolledFill #-}

unrolledFill#
    :: forall a uf. Arity uf
    => uf
    -> (a -> IO ()) -> (Dim1 -> IO a)
    -> (Dim1 -> a -> IO ())
    -> Int# -> Int#
    -> IO ()
{-# INLINE unrolledFill# #-}
unrolledFill# unrollFactor tch get write start# end# =
    let !(I# uf#) = arity unrollFactor
        lim# = end# -# uf#
        {-# INLINE go# #-}
        go# i#
            | i# ># lim# = rest# i#
            | otherwise  = do
                let is :: VecList uf Dim1
                    is = V.generate (+ (I# i#))
                as <- V.mapM get is
                V.mapM_ tch as
                V.zipWithM_ write is as
                go# (i# +# uf#)

        {-# INLINE rest# #-}
        rest# i#
            | i# >=# end# = return ()
            | otherwise   = do
                let i = (I# i#)
                a <- get i
                tch a
                write i a
                rest# (i# +# 1#)

    in go# start#


instance BlockShape Dim1 where
    blockSize (s, e) = e - s
    insideBlock (s, e) i = i >= s && i < e

    clipBlock outer@(os, oe) inner =
        let intersection@(is, ie) = intersectBlocks [inner, outer]
        in if blockSize intersection <= 0
                then [outer]
                else [(os, is), (ie, oe)]

    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE clipBlock #-}




type Dim2 = (Int, Int)
type instance Rank Dim2 = N2

instance Shape Dim2 where
    zero = (0, 0)
    size (h, w) = h * w
    fromIndex (_, w) i = i `quotRem` w
    toIndex (_, w) (y, x) = y * w + x
    
    intersect shapes =
        let (hs, ws) = unzip shapes
        in (P.minimum hs, P.minimum ws)

    complement shapes =
        let (hs, ws) = unzip shapes
        in (P.maximum hs, P.maximum ws)

    makeChunkRange chunks (sy, sx) (ey, ex) =
        let {-# INLINE range #-}
            range = makeChunkRange chunks sy ey
        in \c -> let (csy, cey) = range c in ((csy, sx), (cey, ex))

    unrolledFill unrollFactor tch get write (sy, sx) (ey, ex) =
        let {-# INLINE go #-}
            go y | y >= ey   = return ()
                 | otherwise = do
                    unrolledFill
                        unrollFactor tch
                        (\x -> get (y, x))
                        (\x a -> write (y, x) a)
                        sx ex
                    go (y + 1)
        in go sy

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE fromIndex #-}
    {-# INLINE toIndex #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE makeChunkRange #-}
    {-# INLINE unrolledFill #-}


instance BlockShape Dim2 where
    blockSize ((sy, sx), (ey, ex)) = (ey - sy) * (ex - sx)

    insideBlock ((sy, sx), (ey, ex)) (iy, ix) =
        (iy >= sy && iy < ey) && (ix >= sx && ix < ex)

    clipBlock outer@((osy, osx), (oey, oex)) inner =
        let intersection@((isy, isx), (iey, iex)) =
                intersectBlocks [inner, outer]
        in if blockSize intersection <= 0
                then [outer]
                else [((osy, isx), (isy, oex)),
                      ((isy, iex), (oey, oex)),
                      ((iey, osx), (oey, iex)),
                      ((osy, osx), (iey, isx))]

    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE clipBlock #-}


dim2BlockFill
    :: forall a bsx bsy. (Arity bsx, Arity bsy)
    => bsx                  -- block size by x
    -> bsy                  -- block size by y
    -> (a -> IO ())         -- touch
    -> (Dim2 -> IO a)       -- get
    -> (Dim2 -> a -> IO ()) -- write
    -> Dim2 -> Dim2         -- start, end
    -> IO ()
{-# INLINE dim2BlockFill #-}
dim2BlockFill blockSizeX blockSizeY tch get write =
    \ start@(sy, sx) end@(ey, ex) ->
        let !bx = arity blockSizeX
            !limX = ex - bx

            !by = arity blockSizeY
            !limY = ey - by

            {-# INLINE goY #-}
            goY !y | y > limY  =
                        unrolledFill blockSizeX tch get write (y, sx) end
                   | otherwise = do
                        goX y (y + by)
                        goY (y + by)

            {-# INLINE goX #-}
            goX !y !stripEndY =
                let ys :: VecList bsy Int
                    !ys = V.generate (+ y)

                    {-# INLINE go #-}
                    go !x
                        | x > limX  =
                            unrolledFill n1 tch get write (y, x) (stripEndY, ex)
                        | otherwise = do
                            let xs :: VecList bsx Int
                                xs = V.generate (+ x)
                                is = V.map (\y -> V.map (\x -> (y, x)) xs) ys

                            as <- V.mapM (V.mapM get) is
                            V.mapM_ (V.mapM_ tch) as
                            V.zipWithM_ (V.zipWithM_ write) is as

                            go (x + bx)
                in go sx

        in goY sy




type Dim3 = (Int, Int, Int)
type instance Rank Dim3 = N3

instance Shape Dim3 where
    zero = (0, 0, 0)
    size (d, h, w) = d * h * w
    
    fromIndex (_, h, w) i =
        let (i', x) = i `quotRem` w
            (z, y) = i' `quotRem` h
        in (z, y, x)

    toIndex (_, h, w) (z, y, x) = z * (h * w) + y * w + x

    intersect shapes =
        let (ds, hs, ws) = unzip3 shapes
        in (P.minimum ds, P.minimum hs, P.minimum ws)

    complement shapes =
        let (ds, hs, ws) = unzip3 shapes
        in (P.maximum ds, P.maximum hs, P.maximum ws)

    makeChunkRange chunks (sz, sy, sx) (ez, ey, ex) =
        let {-# INLINE range #-}
            range = makeChunkRange chunks sz ez
        in \c -> let (csz, cez) = range c
                 in ((csz, sy, sx), (cez, ey, ex))

    unrolledFill unrollFactor tch get write (sz, sy, sx) (ez, ey, ex) =
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

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE fromIndex #-}
    {-# INLINE toIndex #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE makeChunkRange #-}
    {-# INLINE unrolledFill #-}
