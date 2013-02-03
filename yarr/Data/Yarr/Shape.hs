
module Data.Yarr.Shape where

import Prelude as P hiding (foldl, foldr)
import GHC.Exts

import Control.DeepSeq

import Data.Yarr.Utils.FixedVector as V hiding (foldl, foldr)
import Data.Yarr.Utils.LowLevelFolds
import Data.Yarr.Utils.Touchable
import Data.Yarr.Utils.Split


class (Eq sh, Show sh, NFData sh) => Shape sh where
    zero :: sh
    size :: sh -> Int
    
    fromIndex :: sh -> Int -> sh
    toIndex :: sh -> sh -> Int
    
    intersect :: [sh] -> sh
    complement :: [sh] -> sh
    intersectBlocks :: [(sh, sh)] -> (sh, sh)
    intersectBlocks blocks =
        let (ss, es) = unzip blocks
        in (complement ss, intersect es)

    blockSize :: (sh, sh) -> Int
    insideBlock :: (sh, sh) -> sh -> Bool

    makeChunkRange :: Int -> sh -> sh -> (Int -> (sh, sh))

    foldl :: (b -> sh -> a -> IO b) -- Generalized reduce
          -> b                      -- Zero
          -> (sh -> IO a)           -- Get
          -> sh -> sh               -- Start, end
          -> IO b                   -- Result

    unrolledFoldl
        :: forall a b uf. Arity uf
        => uf                     -- Unroll factor
        -> (a -> IO ())           -- Touch
        -> (b -> sh -> a -> IO b) -- Generalized reduce
        -> b                      -- Zero
        -> (sh -> IO a)           -- Get
        -> sh -> sh               -- Start, end
        -> IO b                   -- Result

    foldr :: (sh -> a -> b -> IO b) -- Generalized reduce
          -> b                      -- Zero
          -> (sh -> IO a)           -- Get
          -> sh -> sh               -- Start, end
          -> IO b                   -- Result

    unrolledFoldr
        :: forall a b uf. Arity uf
        => uf                     -- Unroll factor
        -> (a -> IO ())           -- Touch
        -> (sh -> a -> b -> IO b) -- Generalized reduce
        -> b                      -- Zero
        -> (sh -> IO a)           -- Get
        -> sh -> sh               -- Start, end
        -> IO b                   -- Result

    fill :: (sh -> IO a)       -- Get
         -> (sh -> a -> IO ()) -- Write
         -> sh -> sh           -- Start, end
         -> IO ()
    fill get write = foldl (\_ sh a -> write sh a) () get

    unrolledFill
        :: forall a uf. Arity uf
        => uf                 -- Unroll factor
        -> (a -> IO ())       -- Touch
        -> (sh -> IO a)       -- Get
        -> (sh -> a -> IO ()) -- Write
        -> sh -> sh           -- Start, end
        -> IO ()
    unrolledFill unrollFactor tch get write =
        unrolledFoldl unrollFactor tch (\_ sh a -> write sh a) () get

    {-# INLINE intersectBlocks #-}
    {-# INLINE fill #-}
    {-# INLINE unrolledFill #-}


class Shape sh => BlockShape sh where
    clipBlock :: (sh, sh) -> (sh, sh) -> [(sh, sh)]



type Dim1 = Int

instance Shape Dim1 where
    zero = 0
    size = id
    fromIndex _ i = i
    toIndex _ i = i
    intersect = P.minimum
    complement = P.maximum

    blockSize (s, e) = e - s
    insideBlock (s, e) i = i >= s && i < e

    makeChunkRange chunks start end =
        let {-# INLINE split #-}
            split = makeSplitIndex chunks start end
        in \ !c -> (split c, split (c + 1))

    foldl reduce z get =
        \ (I# start#) (I# end#) -> foldl# reduce z get start# end#

    unrolledFoldl unrollFactor tch reduce z get =
        \ (I# start#) (I# end#) ->
            unrolledFoldl# unrollFactor tch reduce z get start# end#

    foldr reduce z get =
        \ (I# start#) (I# end#) -> foldr# reduce z get start# end#

    unrolledFoldr unrollFactor tch reduce z get =
        \ (I# start#) (I# end#) ->
            unrolledFoldr# unrollFactor tch reduce z get start# end# 

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE fromIndex #-}
    {-# INLINE toIndex #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE makeChunkRange #-}
    {-# INLINE foldl #-}
    {-# INLINE unrolledFoldl #-}
    {-# INLINE foldr #-}
    {-# INLINE unrolledFoldr #-}


instance BlockShape Dim1 where
    clipBlock outer@(os, oe) inner =
        let intersection@(is, ie) = intersectBlocks [inner, outer]
        in if blockSize intersection <= 0
                then [outer]
                else [(os, is), (ie, oe)]

    {-# INLINE clipBlock #-}




type Dim2 = (Int, Int)

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

    blockSize ((sy, sx), (ey, ex)) = (ey - sy) * (ex - sx)

    insideBlock ((sy, sx), (ey, ex)) (iy, ix) =
        (iy >= sy && iy < ey) && (ix >= sx && ix < ex)

    makeChunkRange chunks (sy, sx) (ey, ex) =
        let {-# INLINE range #-}
            range = makeChunkRange chunks sy ey
        in \c -> let (csy, cey) = range c in ((csy, sx), (cey, ex))


    foldl reduce z get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y >= ey   = return b
                    | otherwise = do
                        b' <- foldl (\b x a -> reduce b (y, x) a) b
                                    (\x -> get (y, x))
                                    sx ex
                        go (y + 1) b'
            in go sy z

    unrolledFoldl unrollFactor tch reduce z get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y >= ey   = return b
                    | otherwise = do
                        b' <- unrolledFoldl
                                unrollFactor tch
                                (\b x a -> reduce b (y, x) a) b
                                (\x -> get (y, x))
                                sx ex
                        go (y + 1) b'
            in go sy z


    foldr reduce z get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y < sy    = return b
                    | otherwise = do
                        b' <- foldr (\x a b -> reduce (y, x) a b) b
                                    (\x -> get (y, x))
                                    sx ex
                        go (y - 1) b'
            in go (ey - 1) z

    unrolledFoldr unrollFactor tch reduce z get =
        \ (!sy, !sx) (!ey, !ex) ->
            let {-# INLINE go #-}
                go y b
                    | y < sy    = return b
                    | otherwise = do
                        b' <- unrolledFoldr
                                unrollFactor tch
                                (\x a b -> reduce (y, x) a b)
                                b
                                (\x -> get (y, x))
                                sx ex
                        go (y - 1) b'
            in go (ey - 1) z

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE fromIndex #-}
    {-# INLINE toIndex #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE makeChunkRange #-}
    {-# INLINE foldl #-}
    {-# INLINE unrolledFoldl #-}
    {-# INLINE foldr #-}
    {-# INLINE unrolledFoldr #-}


instance BlockShape Dim2 where
    clipBlock outer@((osy, osx), (oey, oex)) inner =
        let intersection@((isy, isx), (iey, iex)) =
                intersectBlocks [inner, outer]
        in if blockSize intersection <= 0
                then [outer]
                else [((osy, isx), (isy, oex)),
                      ((isy, iex), (oey, oex)),
                      ((iey, osx), (oey, iex)),
                      ((osy, osx), (iey, isx))]

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


    foldl reduce zero get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z >= ez   = return b
                    | otherwise = do
                        b' <- foldl
                                (\b (y, x) a -> reduce b (z, y, x) a) b
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z + 1) b'
            in go sz zero

    unrolledFoldl unrollFactor tch reduce zero get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z >= ez   = return b
                    | otherwise = do
                        b' <- unrolledFoldl
                                unrollFactor tch
                                (\b (y, x) a -> reduce b (z, y, x) a) b
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z + 1) b'
            in go sz zero


    foldr reduce zero get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z < sz    = return b
                    | otherwise = do
                        b' <- foldr
                                (\(y, x) a b -> reduce (z, y, x) a b) b
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z - 1) b'
            in go (ez - 1) zero

    unrolledFoldr unrollFactor tch reduce zero get =
        \ (!sz, !sy, !sx) (!ez, !ey, !ex) ->
            let {-# INLINE go #-}
                go z b
                    | z < sz    = return b
                    | otherwise = do
                        b' <- unrolledFoldr
                                unrollFactor tch
                                (\(y, x) a b -> reduce (z, y, x) a b) b
                                (\(y, x) -> get (z, y, x))
                                (sy, sx) (ey, ex)
                        go (z - 1) b'
            in go (ez - 1) zero

    {-# INLINE zero #-}
    {-# INLINE size #-}
    {-# INLINE fromIndex #-}
    {-# INLINE toIndex #-}
    {-# INLINE intersect #-}
    {-# INLINE complement #-}
    {-# INLINE blockSize #-}
    {-# INLINE insideBlock #-}
    {-# INLINE makeChunkRange #-}
    {-# INLINE foldl #-}
    {-# INLINE unrolledFoldl #-}
    {-# INLINE foldr #-}
    {-# INLINE unrolledFoldr #-}
