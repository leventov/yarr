
module Data.Yarr.Repr.Boxed where

import Control.Monad.ST (RealWorld)
import Data.Primitive.Array

import Data.Yarr.Base hiding (fmap)
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate

data B

instance (Shape sh, NFData a) => Regular B sh a where

    data UArray B sh a = Boxed !sh !(Array a)

    extent (Boxed sh _) = sh
    shapeIndexingPreferred _ = False
    touch _ = return ()

    {-# INLINE extent #-}
    {-# INLINE shapeIndexingPreferred #-}
    {-# INLINE touch #-}

instance (Shape sh, NFData a) => NFData (UArray B sh a) where
    rnf (Boxed sh !arr) = sh `deepseq` arr `seq` ()

instance (Shape sh, NFData a) => USource B sh a where
    linearIndex (Boxed _ arr) = indexArrayM arr
    {-# INLINE linearIndex #-}

instance (Shape sh, NFData a) => DefaultFusion B D sh a b

instance (Shape sh, Vector v e, NFData e) => UVecSource (SE B) sh B v e


data MB

instance (Shape sh, NFData a) => Regular MB sh a where

    data UArray MB sh a = MutableBoxed !sh !(MutableArray RealWorld a)

    extent (MutableBoxed sh _) = sh
    shapeIndexingPreferred _ = False
    touch _ = return ()

    {-# INLINE extent #-}
    {-# INLINE shapeIndexingPreferred #-}
    {-# INLINE touch #-}

instance (Shape sh, NFData a) => NFData (UArray MB sh a) where
    rnf (MutableBoxed sh !marr) = sh `deepseq` marr `seq` ()

instance (Shape sh, NFData a) => USource MB sh a where
    linearIndex (MutableBoxed _ marr) = readArray marr
    {-# INLINE linearIndex #-}

instance (Shape sh, NFData a) => DefaultFusion MB D sh a b

instance (Shape sh, Vector v e, NFData e) => UVecSource (SE MB) sh MB v e

instance (Shape sh, NFData a) => UTarget MB sh a where
    linearWrite (MutableBoxed _ marr) i x = do
        x `deepseq` return ()
        writeArray marr i x
    {-# INLINE linearWrite #-}

instance (Shape sh, NFData a) => Manifest B MB sh a where
    new sh = fmap (MutableBoxed sh) (newArray (size sh) uninitialized)
    freeze (MutableBoxed sh marr) = fmap (Boxed sh) (unsafeFreezeArray marr)
    thaw (Boxed sh arr) = fmap (MutableBoxed sh) (unsafeThawArray arr)

uninitialized = error "Yarr! Uninitialized element in the boxed array"