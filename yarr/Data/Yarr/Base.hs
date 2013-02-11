
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

    -- * Fusion
    DefaultFusion(..), Fusion(..),

    -- * Manifest and Target classes
    UTarget(..), Manifest(..), UVecTarget(..)

) where

import Prelude as P

import Control.DeepSeq

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
    touchArray :: UArray r l sh a -> IO ()

    -- | /O(1)/ Ensures that array /and all it's real manifest sources/
    -- are fully evaluated.
    -- This function is not for people, it is for GHC compiler.
    --
    -- Default implementation: @force arr = arr \`deepseq\` return ()@
    force :: UArray r l sh a -> IO ()
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


-- | Generalized, non-injective version of 'DefaultFusion'. Used internally.
--
-- Minimum complete defenition: 'fmapM', 'fzip2M', 'fzip3M' and 'fzipM'.
--
-- The class doesn't have vector counterpart, it's role play top-level functions
-- from "Data.Yarr.Repr.Separate" module.
class Fusion r fr l where
    fmap :: (USource r l sh a, USource fr l sh b)
         => (a -> b) -- ^ .
         -> UArray r l sh a -> UArray fr l sh b
    fmap f = fmapM (return . f)
    
    fmapM :: (USource r l sh a, USource fr l sh b)
          => (a -> IO b) -> UArray r l sh a -> UArray fr l sh b

    fzip2 :: (USource r l sh a, USource r l sh b, USource fr l sh c)
          => (a -> b -> c) -- ^ .
          -> UArray r l sh a
          -> UArray r l sh b
          -> UArray fr l sh c
    fzip2 f = fzip2M (\x y -> return (f x y))

    fzip2M :: (USource r l sh a, USource r l sh b, USource fr l sh c)
           => (a -> b -> IO c) -- ^ .
           -> UArray r l sh a
           -> UArray r l sh b
           -> UArray fr l sh c

    fzip3 :: (USource r l sh a, USource r l sh b, USource r l sh c,
              USource fr l sh d)
          => (a -> b -> c -> d) -- ^ .
          -> UArray r l sh a
          -> UArray r l sh b
          -> UArray r l sh c
          -> UArray fr l sh d
    fzip3 f = fzip3M (\x y z -> return (f x y z))

    fzip3M :: (USource r l sh a, USource r l sh b, USource r l sh c,
               USource fr l sh d)
           => (a -> b -> c -> IO d) -- ^ .
           -> UArray r l sh a
           -> UArray r l sh b
           -> UArray r l sh c
           -> UArray fr l sh d

    fzip :: (USource r l sh a, USource fr l sh b, Arity n, n ~ S n0)
         => Fun n a b -- ^ .
         -> VecList n (UArray r l sh a) -> UArray fr l sh b
    fzip fun arrs = let funM = P.fmap return fun in fzipM funM arrs

    fzipM :: (USource r l sh a, USource fr l sh b, Arity n, n ~ S n0)
          => Fun n a (IO b) -- ^ .
          -> VecList n (UArray r l sh a) -> UArray fr l sh b

    {-# INLINE fmap #-}
    {-# INLINE fzip2 #-}
    {-# INLINE fzip3 #-}
    {-# INLINE fzip #-}


-- | This class abstracts pair of array types, which could be (preferably should be)
-- mapped /(fused)/ one to another. Injective version of 'Fusion' class.
-- 
-- Parameters:
--
--  * @r@ - source array representation. It determines result representation.
--
--  * @fr@ (fused repr) - result (fused) array representation. Result array
--    isn't indeed presented in memory, finally it should be
--    'Data.Yarr.Eval.compute'd or 'Data.Yarr.Eval.Load'ed to 'Manifest'
--    representation.
--
--  * @l@ - load type, common for source and fused arrays
--
-- All functions are already defined, using non-injective versions from 'Fusion' class.
--
-- The class doesn't have vector counterpart, it's role play top-level functions
-- from "Data.Yarr.Repr.Separate" module.
class Fusion r fr l => DefaultFusion r fr l | r -> fr where
    -- | /O(1)/ Pure element mapping.
    --
    -- Main basic \"map\" in Yarr.
    dmap :: (USource r l sh a, USource fr l sh b)
         => (a -> b)         -- ^ Element mapper function
         -> UArray r l sh a  -- ^ Source array
         -> UArray fr l sh b -- ^ Result array
    dmap = Data.Yarr.Base.fmap
    
    -- | /O(1)/ Monadic element mapping.
    dmapM :: (USource r l sh a, USource fr l sh b)
          => (a -> IO b)      -- ^ Monadic element mapper function
          -> UArray r l sh a  -- ^ Source array
          -> UArray fr l sh b -- ^ Result array
    dmapM = fmapM

    -- | /O(1)/ Zipping 2 arrays of the same type indexes and shapes.
    -- 
    -- Example:
    -- 
    -- @
    -- let productArr = dzip2 (*) arr1 arr2
    -- @
    dzip2 :: (USource r l sh a, USource r l sh b, USource fr l sh c)
          => (a -> b -> c)     -- ^ Pure element zipper function
          -> UArray r l sh a   -- ^ 1st source array
          -> UArray r l sh b   -- ^ 2nd source array
          -> UArray fr l sh c  -- ^ Fused result array
    dzip2 = fzip2

    -- | /O(1)/ Monadic version of 'dzip2' function.
    dzip2M :: (USource r l sh a, USource r l sh b, USource fr l sh c)
           => (a -> b -> IO c) -- ^ Monadic element zipper function
           -> UArray r l sh a  -- ^ 1st source array
           -> UArray r l sh b  -- ^ 2nd source array
           -> UArray fr l sh c -- ^ Result array
    dzip2M = fzip2M

    -- | /O(1)/ Zipping 3 arrays of the same type indexes and shapes.
    dzip3 :: (USource r l sh a, USource r l sh b, USource r l sh c,
              USource fr l sh d)
          => (a -> b -> c -> d) -- ^ Pure element zipper function
          -> UArray r l sh a    -- ^ 1st source array
          -> UArray r l sh b    -- ^ 2nd source array
          -> UArray r l sh c    -- ^ 3rd source array
          -> UArray fr l sh d   -- ^ Result array
    dzip3 = fzip3

    -- | /O(1)/ Monadic version of 'dzip3' function.
    dzip3M :: (USource r l sh a, USource r l sh b, USource r l sh c,
               USource fr l sh d)
           => (a -> b -> c -> IO d) -- ^ Monadic element zipper function
           -> UArray r l sh a       -- ^ 1st source array
           -> UArray r l sh b       -- ^ 2nd source array
           -> UArray r l sh c       -- ^ 3rd source array
           -> UArray fr l sh d      -- ^ Fused result array
    dzip3M = fzip3M

    -- | /O(1)/ Generalized element zipping with pure function.
    -- Zipper function is wrapped in 'Fun' for injectivity.
    dzip :: (USource r l sh a, USource fr l sh b, Arity n, n ~ S n0)
         => Fun n a b                   -- ^ Wrapped function positionally
                                        -- accepts elements from source arrays
                                        -- and emits element for fused array
         -> VecList n (UArray r l sh a) -- ^ Source arrays
         -> UArray fr l sh b            -- ^ Result array
    dzip = fzip

    -- | /O(1)/ Monadic version of 'dzip' function.
    dzipM :: (USource r l sh a, USource fr l sh b, Arity n, n ~ S n0)
          => Fun n a (IO b)              -- ^ Wrapped monadic zipper
          -> VecList n (UArray r l sh a) -- ^ Source arrays
          -> UArray fr l sh b            -- ^ Result array
    dzipM = fzipM

    {-# INLINE dmap #-}
    {-# INLINE dmapM #-}
    {-# INLINE dzip2 #-}
    {-# INLINE dzip2M #-}
    {-# INLINE dzip3 #-}
    {-# INLINE dzip3M #-}
    {-# INLINE dzip #-}
    {-# INLINE dzipM #-}


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