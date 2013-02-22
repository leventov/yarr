
module Data.Yarr.Utils.FixedVector.VecTupleInstances where

import Data.Vector.Fixed
import Data.Yarr.Utils.FixedVector.Arity
import Data.Yarr.Utils.FixedVector.VecTuple

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
VEC_TUPLE_INST(N7,VT_7,(e, e, e, e, e, e, e))
VEC_TUPLE_INST(N8,VT_8,(e, e, e, e, e, e, e, e))
