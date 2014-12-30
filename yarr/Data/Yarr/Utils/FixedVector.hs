{-# OPTIONS_GHC -Wall                     #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}

module Data.Yarr.Utils.FixedVector (
    -- * Fixed Vector  
    module Data.Vector.Fixed,
    arity,
    
    -- * Missed utility
    zipWith3, zipWithM_, apply, zero,
    iifoldl, iifoldM,

    -- * Aliases and shortcuts
    -- ** Arity
    N7, N8,
    -- | Arity \"instances\" -- aliases to 'undefined'.
    n1, n2, n3, n4, n5, n6, n7, n8,
    
    -- ** VecList makers
    vl_1, vl_2, vl_3, vl_4,

    -- * VecTuple
    VecTuple(..),
    module Data.Yarr.Utils.FixedVector.VecTupleInstances,
    makeVecTupleInstance,

    -- * InlinableArity
    InlinableArity(..), makeInlinableArityInstance,

) where

import Prelude hiding (
    foldl, zipWith, zipWith3,
    all, any, sequence_, replicate)

import Control.DeepSeq

import Data.Vector.Fixed
import Data.Vector.Fixed.Cont ( accum, arity )

import Data.Yarr.Utils.FixedVector.Arity

import Data.Yarr.Utils.FixedVector.VecTuple
import Data.Yarr.Utils.FixedVector.VecTupleInstances

import Data.Yarr.Utils.FixedVector.InlinableArity


vl_1 :: a -> VecList N1 a
{-# INLINE vl_1 #-}
vl_1 a = a `Cons` Nil

vl_2 :: a -> a -> VecList N2 a
{-# INLINE vl_2 #-}
vl_2 a b = a `Cons` (b `Cons` Nil)

vl_3 :: a -> a -> a -> VecList N3 a
{-# INLINE vl_3 #-}
vl_3 a b c = a `Cons` (b `Cons` (c `Cons` Nil))

vl_4 :: a -> a -> a -> a -> VecList N4 a
{-# INLINE vl_4 #-}
vl_4 a b c d = a `Cons` (b `Cons` (c `Cons` (d `Cons` Nil)))


instance (Arity n, NFData e) => NFData (VecList n e) where
    rnf = Data.Vector.Fixed.foldl (\r e -> r `seq` rnf e) ()
    {-# INLINE rnf #-}

    
zipWith3
  :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (b, c))
  => (a -> b -> c -> d)
  -> v a -> v b -> v c
  -> v d
{-# INLINE zipWith3 #-}
zipWith3 f v1 v2 v3 = zipWith (\a (b, c) -> f a b c) v1 (zipWith (,) v2 v3)

zipWithM_
  :: (Vector v a, Vector v b, Vector v c, Monad m, Vector v (m c))
  => (a -> b -> m c) -> v a -> v b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)


apply :: (Vector v a, Vector v (a -> b), Vector v b)
    => v (a -> b) -> v a -> v b
{-# INLINE apply #-}
apply = zipWith ($)

zero :: (Vector v a, Num a) => v a
{-# INLINE zero #-}
zero = replicate 0


iifoldl
    :: Vector v a
    => ix -> (ix -> ix)
    -> (b -> ix -> a -> b) -> b -> v a -> b
{-# INLINE iifoldl #-}
iifoldl st sc f z v = inspect v $ gifoldlF st sc f z

iifoldM
    :: (Vector v a, Monad m)
    => ix -> (ix -> ix)
    -> (b -> ix -> a -> m b) -> b -> v a -> m b
{-# INLINE iifoldM #-}
iifoldM st sc f x v =
    let go m i a = do
            b <- m
            f b i a
    in iifoldl st sc go (return x) v

data T_ifoldl ix b n = T_ifoldl ix b

gifoldlF
    :: forall n ix a b. Arity n
    => ix -> (ix -> ix)
    -> (b -> ix -> a -> b) -> b -> Fun n a b
{-# INLINE gifoldlF #-}
gifoldlF st sc f b = Fun $
    accum (\(T_ifoldl i r) a -> T_ifoldl (sc i) (f r i a))
          (\(T_ifoldl _ r) -> r)
          (T_ifoldl st b :: T_ifoldl ix b n)
