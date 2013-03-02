
module Data.Yarr.Repr.Boxed (
    B, MB,
    -- | There are also @Boxed@  and @MutableBoxed@
    -- 'UArray' family constructors,
    -- which aren't presented in the docs because Haddock
    -- doesn't support associated family constructors.
    --
    -- See source of "Data.Yarr.Repr.Boxed" module.
    UArray(..)
) where

import Control.Monad.ST (RealWorld)
import Data.Primitive.Array

import Data.Yarr.Base
import Data.Yarr.Fusion hiding (fmap)
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Debug.Yarr

-- | 'B'oxed representation is a wrapper for 'Data.Primitive.Array.Array'
-- from @primitive@ package. It may be used to operate with arrays
-- of variable-lengths or multiconstructor ADTs, for example, lists.
-- 
-- For 'Foreign.Storable' element types you would better use
-- 'Data.Yarr.Repr.Foreign.F'oreign arrays.
--
-- /TODO:/ test this representation at least one time...
data B

instance (Shape sh, NFData a) => Regular B L sh a where

    data UArray B L sh a = Boxed !sh !(Array a)

    extent (Boxed sh _) = sh
    touchArray _ = return ()

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}

instance (Shape sh, NFData a) => NFData (UArray B L sh a) where
    rnf (Boxed sh !arr) = sh `deepseq` arr `seq` ()

instance (Shape sh, NFData a) => USource B L sh a where
    linearIndex (Boxed _ arr) = indexArrayM arr
    {-# INLINE linearIndex #-}

instance DefaultFusion B D L sh
instance Shape sh => DefaultIFusion B L D SH sh

instance (Shape sh, Vector v e, NFData e) => UVecSource (SE B) B L sh v e

-- | Mutable Boxed is a wrapper for 'Data.Primitive.Array.MutableArray'.
data MB

instance (Shape sh, NFData a) => Regular MB L sh a where

    data UArray MB L sh a = MutableBoxed !sh !(MutableArray RealWorld a)

    extent (MutableBoxed sh _) = sh
    touchArray _ = return ()

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}

instance (Shape sh, NFData a) => NFData (UArray MB L sh a) where
    rnf (MutableBoxed sh !marr) = sh `deepseq` marr `seq` ()

instance (Shape sh, NFData a) => USource MB L sh a where
    linearIndex (MutableBoxed _ marr) = readArray marr
    {-# INLINE linearIndex #-}

instance DefaultFusion MB D L sh
instance Shape sh => DefaultIFusion MB L D SH sh

instance (Shape sh, Vector v e, NFData e) => UVecSource (SE MB) MB L sh v e

instance (Shape sh, NFData a) => UTarget MB L sh a where
    linearWrite (MutableBoxed _ marr) i x = do
        x `deepseq` return ()
        writeArray marr i x
    {-# INLINE linearWrite #-}

instance (Shape sh, NFData a) => Manifest B MB L sh a where
    new sh = fmap (MutableBoxed sh) (newArray (size sh) uninitialized)
    freeze (MutableBoxed sh marr) = fmap (Boxed sh) (unsafeFreezeArray marr)
    thaw (Boxed sh arr) = fmap (MutableBoxed sh) (unsafeThawArray arr)

uninitialized = yerr "Uninitialized element in the boxed array"