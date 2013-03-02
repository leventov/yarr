-- | Fusion type system. Use re-exported in "Data.Yarr.Flow" functions.
module Data.Yarr.Fusion where

import Prelude as P

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Utils.FixedVector as V

-- | Generalized, non-injective version of 'DefaultFusion'. Used internally.
--
-- Minimum complete defenition: 'fmapM', 'fzip2M', 'fzip3M' and 'fzipM'.
--
-- The class doesn't have vector counterpart, it's role play top-level functions
-- from "Data.Yarr.Repr.Separate" module.
class Fusion r fr l sh where
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
--    'Data.Yarr.Eval.compute'd or 'Data.Yarr.Eval.Load'ed to 'Data.Yarr.Base.Manifest'
--    representation.
--
--  * @l@ - load type, common for source and fused arrays
--
--  * @sh@ - shape of arrays
--
-- All functions are already defined, using non-injective versions from 'Fusion' class.
--
-- The class doesn't have vector counterpart, it's role play top-level functions
-- from "Data.Yarr.Repr.Separate" module.
class Fusion r fr l sh => DefaultFusion r fr l sh | r -> fr where
    -- | /O(1)/ Pure element mapping.
    --
    -- Main basic \"map\" in Yarr.
    dmap :: (USource r l sh a, USource fr l sh b)
         => (a -> b)         -- ^ Element mapper function
         -> UArray r l sh a  -- ^ Source array
         -> UArray fr l sh b -- ^ Result array
    dmap = Data.Yarr.Fusion.fmap
    
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

-- | Like 'Fusion', for mappings/zippings with array index. Used to define
-- functions in 'DefaultIFusion'.
-- 
-- Minimum complete defenition: 'fimapM', 'fizip2M', 'fizip3M' and 'fizipM'.
--
-- The class doesn't have vector counterpart.
class PreferredWorkIndex fl sh sh => IFusion r l fr fl sh | r l fr -> fl where
    fimap :: (USource r l sh a, USource fr fl sh b)
          => (sh -> a -> b) -- ^ .
          -> UArray r l sh a -> UArray fr fl sh b
    fimap f = fimapM (\i x -> return (f i x))
    
    fimapM :: (USource r l sh a, USource fr fl sh b)
           => (sh -> a -> IO b) -> UArray r l sh a -> UArray fr fl sh b

    fizip2 :: (USource r l sh a, USource r l sh b, USource fr fl sh c)
           => (sh -> a -> b -> c) -- ^ .
           -> UArray r l sh a
           -> UArray r l sh b
           -> UArray fr fl sh c
    fizip2 f = fizip2M (\i x y -> return (f i x y))

    fizip2M :: (USource r l sh a, USource r l sh b, USource fr fl sh c)
            => (sh -> a -> b -> IO c) -- ^ .
            -> UArray r l sh a
            -> UArray r l sh b
            -> UArray fr fl sh c

    fizip3 :: (USource r l sh a, USource r l sh b, USource r l sh c,
               USource fr fl sh d)
           => (sh -> a -> b -> c -> d) -- ^ .
           -> UArray r l sh a
           -> UArray r l sh b
           -> UArray r l sh c
           -> UArray fr fl sh d
    fizip3 f = fizip3M (\i x y z -> return (f i x y z))

    fizip3M :: (USource r l sh a, USource r l sh b, USource r l sh c,
                USource fr fl sh d)
            => (sh -> a -> b -> c -> IO d) -- ^ .
            -> UArray r l sh a
            -> UArray r l sh b
            -> UArray r l sh c
            -> UArray fr fl sh d

    fizip :: (USource r l sh a, USource fr fl sh b, Arity n, n ~ S n0)
          => (sh -> Fun n a b) -- ^ .
          -> VecList n (UArray r l sh a) -> UArray fr fl sh b
    fizip fun arrs = fizipM funM arrs
      where funM i = P.fmap return (fun i)

    fizipM :: (USource r l sh a, USource fr fl sh b, Arity n, n ~ S n0)
           => (sh -> Fun n a (IO b)) -- ^ .
           -> VecList n (UArray r l sh a) -> UArray fr fl sh b

    {-# INLINE fimap #-}
    {-# INLINE fizip2 #-}
    {-# INLINE fizip3 #-}
    {-# INLINE fizip #-}

-- | Like 'DefaultFusion', this class abstracts the pair array types,
-- which should be fused one to another
-- on maps and zips which accept index of element
-- (several elements for zips) in array (arrays).
--
-- Parameters:
--
--  * @r@ - source array representation. Determines result representation.
--
--  * @l@ - source load type
--
--  * @fr@ (fused repr) - result (fused) array representation. Result array
--    isn't indeed presented in memory, finally it should be
--    'Data.Yarr.Eval.compute'd or 'Data.Yarr.Eval.Load'ed to 'Data.Yarr.Base.Manifest'
--    representation.
--
--  * @fl@ - result, \"shaped\" load type.
--
--  * @sh@ - shape of arrays
--
-- All functions are already defined,
-- using non-injective versions from 'IFusion' class.
--
-- The class doesn't have vector counterpart.
class IFusion r l fr fl sh => DefaultIFusion r l fr fl sh | r l -> fr where
    -- | /O(1)/ Pure element mapping with array index.
    imap :: (USource r l sh a, USource fr fl sh b)
         => (sh -> a -> b)    -- ^ Indexed mapping function
         -> UArray r l sh a   -- ^ Source array
         -> UArray fr fl sh b -- ^ Fused result array
    imap = fimap
    
    -- | /O(1)/ Monadic element mapping with index.
    imapM :: (USource r l sh a, USource fr fl sh b)
          => (sh -> a -> IO b) -- ^ Indexed monadic mapping function
          -> UArray r l sh a   -- ^ Source array
          -> UArray fr fl sh b -- ^ Result fused array
    imapM = fimapM

    -- | /O(1)/ Pure zipping of 2 arrays with index.
    izip2 :: (USource r l sh a, USource r l sh b, USource fr fl sh c)
          => (sh -> a -> b -> c) -- ^ Indexed zipping function
          -> UArray r l sh a     -- ^ 1st source array
          -> UArray r l sh b     -- ^ 2nd source array
          -> UArray fr fl sh c   -- ^ Fused result array
    izip2 = fizip2

    -- | /O(1)/ Monadic zipping of 2 arrays with index.
    izip2M :: (USource r l sh a, USource r l sh b, USource fr fl sh c)
           => (sh -> a -> b -> IO c) -- ^ Indexed monadic zipping function
           -> UArray r l sh a        -- ^ 1st source array
           -> UArray r l sh b        -- ^ 2nd source array
           -> UArray fr fl sh c      -- ^ Fused result array
    izip2M = fizip2M

    -- | /O(1)/ Pure zipping of 3 arrays with index.
    izip3 :: (USource r l sh a, USource r l sh b, USource r l sh c,
               USource fr fl sh d)
          => (sh -> a -> b -> c -> d) -- ^ Indexed zipping function
          -> UArray r l sh a          -- ^ 1st source array
          -> UArray r l sh b          -- ^ 2nd source array
          -> UArray r l sh c          -- ^ 3rd source array
          -> UArray fr fl sh d        -- ^ Fused result array
    izip3 = fizip3

    -- | /O(1)/ Monadic zipping of 3 arrays with index.
    izip3M :: (USource r l sh a, USource r l sh b, USource r l sh c,
                USource fr fl sh d)
           => (sh -> a -> b -> c -> IO d) -- ^ Indexed monadic zipping function
           -> UArray r l sh a             -- ^ 1st source array
           -> UArray r l sh b             -- ^ 2nd source array
           -> UArray r l sh c             -- ^ 3rd source array
           -> UArray fr fl sh d           -- ^ Fused result array
    izip3M = fizip3M

    -- | /O(1)/ Generalized pure element zipping with index in arrays.
    -- Zipper function is wrapped in 'Fun' for injectivity.
    izip :: (USource r l sh a, USource fr fl sh b, Arity n, n ~ S n0)
         => (sh -> Fun n a b)           -- ^ Accepts index in array and returns
                                        -- wrapped zipper, which positionally
                                        -- accepts elements from source arrays
                                        -- and emits element for the result array
         -> VecList n (UArray r l sh a) -- ^ Bunch of source arrays
         -> UArray fr fl sh b           -- ^ Result fused array
    izip = fizip

    -- | /O(1)/ Monadic version of 'izip' function.
    izipM :: (USource r l sh a, USource fr fl sh b, Arity n, n ~ S n0)
          => (sh -> Fun n a (IO b))      -- ^ Monadic indexed zipper
          -> VecList n (UArray r l sh a) -- ^ Source arrays
          -> UArray fr fl sh b           -- ^ Result fused array
    izipM = fizipM

    {-# INLINE imap #-}
    {-# INLINE imapM #-}
    {-# INLINE izip2 #-}
    {-# INLINE izip2M #-}
    {-# INLINE izip3 #-}
    {-# INLINE izip3M #-}
    {-# INLINE izip #-}
    {-# INLINE izipM #-}


instance IFusion r l fr l sh => Fusion r fr l sh where
   fmapM f = fimapM (\_ x -> f x)
   fzip2M f = fizip2M (\_ x y -> f x y)
   fzip3M f = fizip3M (\_ x y z -> f x y z)
   fzipM funM = fizipM (\_ -> funM)
   {-# INLINE fmapM #-}
   {-# INLINE fzip2M #-}
   {-# INLINE fzip3M #-}
   {-# INLINE fzipM #-}