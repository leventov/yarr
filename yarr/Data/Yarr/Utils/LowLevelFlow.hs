
module Data.Yarr.Utils.LowLevelFlow where

import GHC.Exts
import Data.Yarr.Utils.FixedVector as V


fill# :: (Int -> IO a)
      -> (Int -> a -> IO ())
      -> Int# -> Int#
      -> IO ()
{-# INLINE fill# #-}
fill# get write start# end# =
    let {-# INLINE go# #-}
        go# i#
            | isTrue# (i# >=# end#) = return ()
            | otherwise   = do
                let i = (I# i#)
                a <- get i
                write i a
                go# (i# +# 1#)
    in go# start#
    
unrolledFill#
    :: forall a uf. Arity uf
    => uf
    -> (a -> IO ())
    -> (Int -> IO a)
    -> (Int -> a -> IO ())
    -> Int# -> Int#
    -> IO ()
{-# INLINE unrolledFill# #-}
unrolledFill# unrollFactor tch get write start# end# =
    let !(I# uf#) = arity unrollFactor
        lim# = end# -# uf#
        {-# INLINE go# #-}
        go# i#
            | isTrue# (i# ># lim#) = fill# get write i# end#
            | otherwise  = do
                let is :: VecList uf Int
                    is = V.generate (+ (I# i#))
                as <- V.mapM get is
                V.mapM_ tch as
                V.zipWithM_ write is as
                go# (i# +# uf#)
                
    in go# start#


foldl#
    :: (b -> Int -> a -> IO b)
    -> IO b
    -> (Int -> IO a)
    -> Int# -> Int#
    -> IO b
{-# INLINE foldl# #-}
foldl# reduce mz get start# end# =
    let {-# INLINE go# #-}
        go# !b i#
            | isTrue# (i# >=# end#) = return b
            | otherwise   = do
                let i = (I# i#)
                a <- get i
                b' <- reduce b i a
                go# b' (i# +# 1#)
    in do z <- mz
          go# z start#

unrolledFoldl#
    :: forall a b uf. Arity uf
    => uf
    -> (a -> IO ())
    -> (b -> Int -> a -> IO b)
    -> IO b
    -> (Int -> IO a)
    -> Int# -> Int#
    -> IO b
{-# INLINE unrolledFoldl# #-}
unrolledFoldl# unrollFactor tch reduce mz get start# end# =
    let !(I# uf#) = arity unrollFactor
        lim# = end# -# uf#
        {-# INLINE go# #-}
        go# !b i#
            | isTrue# (i# ># lim#) = rest# b i#
            | otherwise  = do
                let is :: VecList uf Int
                    is = V.generate (+ (I# i#))
                as <- V.mapM get is
                V.mapM_ tch as
                b' <- V.foldM
                        (\b (i, a) -> reduce b i a) b
                        (V.zipWith (,) is as)
                go# b' (i# +# uf#)

        {-# INLINE rest# #-}
        rest# !b i#
            | isTrue# (i# >=# end#) = return b
            | otherwise   = do
                let i = (I# i#)
                a <- get i
                tch a
                b' <- reduce b i a
                rest# b' (i# +# 1#)

    in do z <- mz
          go# z start#


foldr#
    :: (Int -> a -> b -> IO b)
    -> IO b
    -> (Int -> IO a)
    -> Int# -> Int#
    -> IO b
{-# INLINE foldr# #-}
foldr# reduce mz get start# end# =
    let {-# INLINE go# #-}
        go# !b i#
            | isTrue# (i# <# start#) = return b
            | otherwise    = do
                let i = (I# i#)
                a <- get i
                b' <- reduce i a b
                go# b' (i# -# 1#)
    in do z <- mz
          go# z (end# -# 1#)

unrolledFoldr#
    :: forall a b uf. Arity uf
    => uf
    -> (a -> IO ())
    -> (Int -> a -> b -> IO b)
    -> IO b
    -> (Int -> IO a)
    -> Int# -> Int#
    -> IO b
{-# INLINE unrolledFoldr# #-}
unrolledFoldr# unrollFactor tch reduce mz get start# end# =
    let !(I# uf#) = arity unrollFactor
        lim# = start# +# uf# -# 1#
        {-# INLINE go# #-}
        go# !b i#
            | isTrue# (i# <# lim#) = rest# b i#
            | otherwise  = do
                let is :: VecList uf Int
                    is = V.generate ((I# i#) -)
                as <- V.mapM get is
                V.mapM_ tch as
                b' <- V.foldM
                        (\b (i, a) -> reduce i a b) b
                        (V.zipWith (,) is as)
                go# b' (i# -# uf#)

        {-# INLINE rest# #-}
        rest# !b i#
            | isTrue# (i# <# start#) = return b
            | otherwise    = do
                let i = (I# i#)
                a <- get i
                tch a
                b' <- reduce i a b
                rest# b' (i# -# 1#)

    in do z <- mz
          go# z (end# -# 1#)

