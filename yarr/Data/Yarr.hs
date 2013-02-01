
module Data.Yarr (
    module Data.Yarr.Base,
    
    Fun(..), Vector(..), VecList(VecList),
    N1, N2, N3, N4, N5, N6,
    
    Dim1, Dim2, Dim3,

    module Data.Yarr.Eval,
    module Data.Yarr.Flow,
    
    F, unsafeFromForeignPtr, toForeignPtr,
    D, fromShapeFunction, fromLinearFunction,
    SE, fromSlices, mapSlices

) where

import Data.Yarr.Base
import Data.Yarr.Eval
import Data.Yarr.Flow
import Data.Yarr.Shape
import Data.Yarr.Repr.Foreign
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V