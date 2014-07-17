
-- | Core type system
module Data.Yarr.Base (

    -- * General Regular classes
    Regular(..), VecRegular(..),
    NFData(..), deepseq,

    -- * Shape class
    Shape,

    -- * Fixed vector
    Dim, Arity, Fun, Vector, VecList,

    -- * Source classes
    USource(..),
    UVecSource(..),

    -- * Manifest and Target classes
    UTarget(..), Manifest(..), UVecTarget(..),

    -- * Work index
    PreferredWorkIndex(..), WorkIndex(..),

) where

import Prelude as P

import Control.DeepSeq
import Control.Monad.IO.Class

import Data.Yarr.Shape as S
import Data.Yarr.Utils.FixedVector as V

import Data.Yarr.Utils.Primitive

-- | This class generalizes 'USource' and 'UTarget'.
--
-- Paramenters:
--
--  * @r@ - representation,
--
--  * @l@ - load type,
--
--  * @sh@ - shape,
--
--  * @a@ - element type.
--
-- Counterpart for arrays of vectors: 'VecRegular'.
class (NFData (UArray r l sh a), Shape sh) => Regular r l sh a where

    data UArray r l sh a

    -- | Returns the extent an the array.
    extent :: UArray r l sh a -> sh

    -- | Calling this function on foreign array ('Data.Yarr.Repr.Foreign.F')
    -- ensures it is still alive (GC haven't picked it).
    -- In other manifest representations, the function defined as @return ()@.
    -- 'touchArray' is lifted to top level in class hierarchy
    -- because in fact foreign representation is the heart of the library.
    touchArray :: MonadIO m => UArray r l sh a -> m ()

    -- | /O(1)/ Ensures that array /and all it's real manifest sources/
    -- are fully evaluated.
    -- This function is not for people, it is for GHC compiler.
    --
    -- Default implementation: @force arr = arr \`deepseq\` return ()@
    force :: MonadIO m => UArray r l sh a -> m ()
    force arr = arr `deepseq` return ()
    {-# INLINE force #-}

-- | Class for arrays of vectors.
--
-- Paramenters:
--
--  * @r@ - (entire) representation.
--    Associated array type for this class is @'UArray' r sh (v e)@.
--
--  * @slr@ - slice representation
--
--  * @l@ - load type
--
--  * @sh@ - shape
--
--  * @v@ - vector type
--
--  * @e@ - /vector/ (not array) element type.
--    Array element type is entire vector: @(v e)@.
--
-- Counterpart for \"simple\" arrays: 'Regular'.
class (Regular r l sh (v e), Regular slr l sh e, Vector v e) =>
        VecRegular r slr l sh v e | r -> slr where

    -- | /O(1)/ Array of vectors -> vector of arrays.
    -- Think about this function as shallow 'Prelude.unzip' from Prelude.
    -- Slices are /views/ of an underlying array.
    --
    -- Example:
    --
    -- @
    -- let css = slices coords
    --     xs = css 'V.!' 0
    --     ys = css 'V.!' 1
    -- @
    slices :: UArray r l sh (v e) -> VecList (Dim v) (UArray slr l sh e)

-- | Class for arrays which could be indexed.
--
--
-- It's functions are unsafe: you /must/ call 'touchArray' after the last call.
-- Fortunately, you will hardly ever need to call them manually.
--
-- Minimum complete defenition: 'index' or 'linearIndex'.
--
-- Counterpart for arrays of vectors: 'UVecSource'
class Regular r l sh a => USource r l sh a where

    -- | Shape, genuine monadic indexing.
    --
    -- In Yarr arrays are always 'zero'-indexed and multidimensionally square.
    -- Maximum index is @(extent arr)@.
    --
    -- Default implementation:
    -- @index arr sh = linearIndex arr $ 'toLinear' ('extent' arr) sh@
    index :: UArray r l sh a -> sh -> IO a
    index arr sh = linearIndex arr $ toLinear (extent arr) sh

    -- | \"Surrogate\" linear index.
    -- For 'Dim1' arrays @index == linearIndex@.
    --
    -- Default implementation:
    -- @linearIndex arr i = index arr $ 'fromLinear' ('extent' arr) i@
    linearIndex :: UArray r l sh a -> Int -> IO a
    linearIndex arr i = index arr $ fromLinear (extent arr) i

    {-# INLINE index #-}
    {-# INLINE linearIndex #-}

-- | Class for arrays of vectors which could be indexed.
-- The class doesn't need to define functions, it just gathers it's dependencies.
--
-- Counterpart for \"simple\" arrays: 'USource'.
class (VecRegular r slr l sh v e, USource r l sh (v e), USource slr l sh e) =>
        UVecSource r slr l sh v e


-- | Class for mutable arrays.
--
-- Just like for 'USource', it's function are unsafe
-- and require calling 'touchArray' after the last call.
--
-- Minimum complete defenition: 'write' or 'linearWrite'.
--
-- Counterpart for arrays of vectors: 'UVecTarget'
class Regular tr tl sh a => UTarget tr tl sh a where
    -- | Shape, genuine monadic writing.
    --
    -- Default implementation:
    -- @write tarr sh = linearWrite tarr $ 'toLinear' ('extent' tarr) sh@
    write :: UArray tr tl sh a -> sh -> a -> IO ()
    write tarr sh = linearWrite tarr $ toLinear (extent tarr) sh

    -- | Fast (usually), linear indexing. Intented to be used internally.
    --
    -- Default implementation:
    -- @linearWrite tarr i = write tarr $ 'fromLinear' ('extent' tarr) i@
    linearWrite :: UArray tr tl sh a -> Int -> a -> IO ()
    linearWrite tarr i = write tarr $ fromLinear (extent tarr) i

    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

-- | Class for arrays which could be created.
-- It combines a pair of representations: freezed and mutable (raw).
-- This segregation is lifted from Boxed representation
-- and, in the final, from GHC system of primitive arrays.
--
-- Parameters:
--
--  * @r@ - freezed array representation.
--
--  * @mr@ - mutable, raw array representation
--
--  * @l@ - load type index, common for both representations
--
--  * @sh@ - shape of arrays
--
--  * @a@ - element type
class (USource r l sh a, UTarget mr l sh a) =>
        Manifest r mr l sh a | r -> mr, mr -> r where

    -- | /O(1)/ Creates and returns mutable array of the given shape.
    new :: sh -> IO (UArray mr l sh a)

    -- | /O(1)/ Freezes mutable array and returns array which could be indexed.
    freeze :: UArray mr l sh a -> IO (UArray r l sh a)

    -- | /O(1)/ Thaws freezed array and returns mutable version.
    thaw :: UArray r l sh a -> IO (UArray mr l sh a)

-- | Class for mutable arrays of vectors.
-- The class doesn't need to define functions, it just gathers it's dependencies.
--
-- Counterpart for \"simple\" arrays: 'UTarget'.
class (VecRegular tr tslr tl sh v e,
       UTarget tr tl sh (v e), UTarget tslr tl sh e) =>
        UVecTarget tr tslr tl sh v e


-- | Internal implementation class. Generalizes @linear-@ and simple
-- indexing and writing function in 'USource' and 'UTarget' classes.
class (Shape sh, Shape i) => WorkIndex sh i where
    toWork :: sh -> i
    gindex :: USource r l sh a => UArray r l sh a -> i -> IO a
    gwrite :: UTarget tr tl sh a => UArray tr tl sh a -> i -> a -> IO ()
    gsize :: USource r l sh a => UArray r l sh a -> i
    gsize = toWork. extent
    {-# INLINE gsize #-}

instance Shape sh => WorkIndex sh sh where
    toWork = id
    gindex = index
    gwrite = write
    {-# INLINE gindex #-}
    {-# INLINE gwrite #-}


#define WI_INT_INST(sh)           \
instance WorkIndex sh Int where { \
    toWork = size;                \
    gindex = linearIndex;         \
    gwrite = linearWrite;         \
    {-# INLINE toWork #-};        \
    {-# INLINE gindex #-};        \
    {-# INLINE gwrite #-};        \
}

WI_INT_INST(Dim2)
WI_INT_INST(Dim3)

-- | Type level fixation of preferred work (load, fold, etc.)
-- index type of the array load type.
--
-- Parameters:
--
--  * @l@ - load type index
--
--  * @sh@ - shape of arrays
--
--  * @i@ - preferred work index, @Int@ or @sh@ itself
class WorkIndex sh i => PreferredWorkIndex l sh i | l sh -> i
