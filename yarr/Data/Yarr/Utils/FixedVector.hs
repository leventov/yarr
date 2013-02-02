
module Data.Yarr.Utils.FixedVector (
    module Data.Vector.Fixed,
    Fn, arity,
    
    N7, N8,
    n1, n2, n3, n4, n5, n6, n7, n8,
    VecTuple(..),
    makeVecTupleInstance,

    zipWith3, zipWithM_, apply, 
    
    iifoldl, iifoldM
) where

import Prelude hiding (zipWith, zipWith3)

import Control.DeepSeq

import Data.Vector.Fixed
import Data.Vector.Fixed.Internal hiding (apply)

import Data.Yarr.Utils.VecTuple

n1 :: N1
n1 = undefined

n2 :: N2
n2 = undefined

n3 :: N3
n3 = undefined

n4 :: N4
n4 = undefined

n5 :: N5
n5 = undefined

n6 :: N6
n6 = undefined

type N7 = S N6
n7 :: N7
n7 = undefined

type N8 = S N7
n8 :: N8
n8 = undefined


#define DERIV(n,clas) deriving instance clas e => clas (VecTuple (n) e)

#define VEC_TUPLE_INST(N,con,tup)               \
makeVecTupleInstance [t|N|] (undefined :: N);   \
DERIV(N, Eq); DERIV(N, Ord); DERIV(N, Bounded); \
DERIV(N, Read); DERIV(N, Show)

VEC_TUPLE_INST(N2,VT_2,(e, e))
VEC_TUPLE_INST(N3,VT_3,(e, e, e))
VEC_TUPLE_INST(N4,VT_4,(e, e, e, e))
VEC_TUPLE_INST(N5,VT_5,(e, e, e, e, e))
VEC_TUPLE_INST(N6,VT_6,(e, e, e, e, e, e))
VEC_TUPLE_INST(S N6,VT_7,(e, e, e, e, e, e, e))
VEC_TUPLE_INST(S (S N6),VT_8,(e, e, e, e, e, e, e, e))


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
zipWithM_ f xs ys = (zipWithM f xs ys) >> return ()


apply :: (Vector v a, Vector v (a -> b), Vector v b)
    => v (a -> b) -> v a -> v b
{-# INLINE apply #-}
apply = Data.Vector.Fixed.zipWith ($)


iifoldl
    :: Vector v a
    => ix -> (ix -> ix)
    -> (b -> ix -> a -> b) -> b -> v a -> b
{-# INLINE iifoldl #-}
iifoldl st sc f z v = inspectV v $ gifoldlF st sc f z

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
