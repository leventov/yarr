
-- | 'Load'ing and computing arrays
module Data.Yarr.Eval (
    -- * Aliases for common parameters
    Threads, caps, threads,
    Fill,

    -- * Load classes
    Load(..), RangeLoad(..),
    VecLoad(..), RangeVecLoad(..),

    -- * Compute functions
    compute,
    dComputeP, dComputeS,

    -- * Common load types
    L, SH,

    -- * Utility
    entire
) where

import GHC.Conc

import Data.Yarr.Base as B
import Data.Yarr.Shape as S

import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel
import Data.Yarr.Utils.Primitive as P

-- | There are 2 common ways to parameterize
-- parallelism: a) to say \"split this work between @n@ threads\"
-- or b) to say \"split this work between maximum reasonable
-- number of threads\", that is /capabilities/. Since
-- 'GHC.Conc.getNumCapabilities' function is monadic, we need always pass
-- @IO Int@ as thread number parameter in order not to multiply
-- number of functions in this module (there are already too many).
type Threads = IO Int

-- | Alias to 'GHC.Conc.getNumCapabilities'.
caps :: Threads
caps = getNumCapabilities

-- | Alias to 'return'.
threads :: Int -> Threads
{-# INLINE threads #-}
threads = return


-- | This class abstracts pair of array types,
-- which could be loaded one to another.
--
-- Parameters:
--
--  * @r@ - source representation. Instance of 'USource' class.
--          Typically one of fused representations:
--          'Data.Yarr.D', @('Data.Yarr.SE' 'Data.Yarr.D')@ or
--          'Data.Yarr.Convolution.Repr.CV'.
--
--  * @l@ - source load type
--
--  * @tr@ - target representation. Instance of 'UTarget' class.
--
--  * @tl@ - target load type
--
--  * @sh@ - shape of arrays
--
--  * @a@ - array element type
--
-- Counterpart for arrays of vectors: 'VecLoad'.
--
-- /TODO:/ this class seems to be overengineered, normally
-- it should have only 3 parameters: @Load l tl sh@.
-- But Convoluted ('Data.Yarr.Convolution.Repr.CV') representation is
-- tightly connected with it's load type.
class (USource r l sh a, UTarget tr tl sh a,
       WorkIndex sh (LoadIndex l tl sh)) =>
        Load r l tr tl sh a where
    -- | Used in @fill@ parameter function.
    -- There are two options for this type to be: @sh@ itself or @Int@.
    -- Don't confuse this type with /load type indexes/: @r@ and @l@.
    -- There are 2 different meanings of word \"index\": data type index
    -- (haskell term) and array index (linear, shape).
    type LoadIndex l tl sh

    -- | /O(n)/ Entirely loads source to target in parallel.
    --
    -- First parameter is used to parameterize loop
    -- unrolling to maximize performance.
    -- Default choice is 'S.fill' -- vanilla not unrolled looping.
    -- 
    -- Examples:
    --
    -- @
    -- tarr <- 'B.new' ('extent' arr)
    -- loadP 'S.fill' 'caps' arr tarr
    -- loadP ('S.dim2BlockFill' 'n2' 'n2' 'P.touch') ('threads' 2) arr tarr
    -- @
    loadP :: Fill (LoadIndex l tl sh) a -- ^ Filling (real worker) function
          -> Threads                    -- ^ Number of threads to parallelize loading on
          -> UArray r l sh a            -- ^ Source array
          -> UArray tr tl sh a          -- ^ Target array
          -> IO ()
    loadP fill threads arr tarr = do
        force arr
        force tarr
        !ts <- threads
        parallel_ ts $
            makeFork ts zero (gsize arr) (fill (gindex arr) (gwrite tarr))
        touchArray arr
        touchArray tarr

    -- | /O(n)/ Sequential analog of 'loadP' function.
    -- Loads source to target 'entire'ly.
    -- 
    -- Example:
    --
    -- @loadS ('S.unrolledFill' 'n4' 'noTouch') 'caps' arr tarr@
    loadS :: Fill (LoadIndex l tl sh) a -- ^ Filling (real worker) function
          -> UArray r l sh a            -- ^ Source array
          -> UArray tr tl sh a          -- ^ Target array
          -> IO ()
    loadS fill arr tarr = do
        force arr
        force tarr
        fill (gindex arr) (gwrite tarr) zero (gsize arr)
        touchArray arr
        touchArray tarr

    {-# INLINE loadP #-}
    {-# INLINE loadS #-}


-- | Class abstracts pair of arrays which could be loaded in
-- just specified range of indices.
--
-- \"Range\" is a multidimensional
-- segment: segment for 'Dim1' arrays, square for 'Dim2' arrays and
-- cube for 'Dim3'. Thus, it is specified by pair of indices:
-- \"top-left\" (minimum is 'zero') and \"bottom-right\" (maximum is
-- @('entire' arr tarr)@) corners.
class Load r l tr tl sh a => RangeLoad r l tr tl sh a where

    -- | /O(n)/ Loads elements from source to target in specified range
    -- in parallel.
    -- 
    -- Example:
    --
    -- @
    -- let ext = extent convolved
    -- res <- new ext
    -- rangeLoadP 'fill' 'caps' convolved res (5, 5) (ext \`minus\` (5, 5))
    -- @
    rangeLoadP
        :: Fill sh a         -- ^ Filling (real worker) function
        -> Threads           -- ^ Number of threads to parallelize loading on
        -> UArray r l sh a   -- ^ Source array
        -> UArray tr tl sh a -- ^ Target array
        -> sh                -- ^ Top-left 
        -> sh                -- ^ and bottom-right corners of range to load
        -> IO ()
    rangeLoadP fill threads arr tarr start end = do
        force arr
        force tarr
        !ts <- threads
        parallel_ ts $
            makeFork ts start end (fill (index arr) (write tarr))
        touchArray arr
        touchArray tarr

    -- | /O(n)/ Sequentially loads elements from source to target in specified range.
    rangeLoadS
        :: Fill sh a         -- ^ Filling (real worker) function
        -> UArray r l sh a   -- ^ Source array
        -> UArray tr tl sh a -- ^ Target array
        -> sh                -- ^ Top-left
        -> sh                -- ^ and bottom-right corners of range to load
        -> IO ()
    rangeLoadS fill arr tarr start end = do
        force arr
        force tarr
        fill (index arr) (write tarr) start end
        touchArray arr
        touchArray tarr

    {-# INLINE rangeLoadP #-}
    {-# INLINE rangeLoadS #-}




-- | Class abstracts /separated in time and space/ loading 'slices' of one array type
-- to another. Result of running functions with @-Slices-@ infix
-- /is always identical/ to result of running corresponding function from
-- 'Load' class. 'VecLoad' and 'RangeVecLoad' are just about performance.
-- If target representation is separate (ex. @('Data.Yarr.SE' 'Data.Yarr.F')@),
-- using 'loadSlicesP' may be faster than 'loadP' because of per-thread memory
-- locality.
--
-- Parameters:
--
--  * @r@ - source representation
--
--  * @slr@ - source slice representation
--
--  * @l@ - source load type 
--
--  * @tr@ - target representation
--
--  * @tslr@ - target slice representation
--
--  * @tl@ - target load type
--
--  * @sh@ - shape of arrays
--
--  * @v@ - source vector type
--
--  * @v2@ - target vector type
--
--  * @e@ - vector element type, common for source and target arrays
--
class (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e,
       Load slr l tslr tl sh e, Dim v ~ Dim v2) =>
        VecLoad r slr l tr tslr tl sh v v2 e where

    -- | /O(n)/ Entirely, slice-wise loads vectors from source to target 
    -- in parallel.
    -- 
    -- Example:
    --
    -- @
    -- -- blurred and delayedBlurred are arrays of color components.
    -- loadSlicesP 'fill' 'caps' delayedBlurred blurred
    -- @
    loadSlicesP
        :: Fill (LoadIndex l tl sh) e -- ^ Fill function to work /on slices/
        -> Threads                    -- ^ Number of threads to parallelize loading on
        -> UArray r l sh (v e)        -- ^ Source array of vectors
        -> UArray tr tl sh (v2 e)     -- ^ Target array of vectors
        -> IO ()
    loadSlicesP fill threads arr tarr = do
        force arr
        force tarr
        !ts <- threads
        parallel_ ts $
            makeForkSlicesOnce
                ts (V.replicate (zero, gsize arr))
                (V.zipWith
                    (\sl tsl -> fill (gindex sl) (gwrite tsl))
                    (slices arr) (slices tarr))
        touchArray arr
        touchArray tarr

    -- | /O(n)/ Sequentially loads vectors from source to target, slice by slice.
    loadSlicesS
        :: Fill (LoadIndex l tl sh) e -- ^ Fill function to work /on slices/
        -> UArray r l sh (v e)        -- ^ Source array of vectors
        -> UArray tr tl sh (v2 e)     -- ^ Target array of vectors
        -> IO ()
    loadSlicesS fill arr tarr = do
        force arr
        force tarr
        V.zipWithM_ (loadS fill) (slices arr) (slices tarr)
        touchArray arr
        touchArray tarr

    {-# INLINE loadSlicesP #-}
    {-# INLINE loadSlicesS #-}


-- | This class extends 'VecLoad' just like 'RangeLoad' extends 'Load'.
-- It abstracts slice-wise loading from one array type to
-- another in specified range.
class (VecLoad r slr l tr tslr tl sh v v2 e, RangeLoad slr l tslr tl sh e) =>
        RangeVecLoad r slr l tr tslr tl sh v v2 e where

    -- | /O(n)/ Loads vectors from source to target in specified range, slice-wise,
    -- in parallel.
    rangeLoadSlicesP
        :: Fill sh e              -- ^ Fill function to work /on slices/
        -> Threads                -- ^ Number of threads to parallelize loading on
        -> UArray r l sh (v e)    -- ^ Source array of vectors
        -> UArray tr tl sh (v2 e) -- ^ Target array of vectors
        -> sh                     -- ^ Top-left
        -> sh                     -- ^ and bottom-right corners of range to load
        -> IO ()
    rangeLoadSlicesP fill threads arr tarr start end = do
        force arr
        force tarr
        !ts <- threads
        parallel_ ts $
            makeForkSlicesOnce
                ts (V.replicate (start, end))
                (V.zipWith
                    (\sl tsl -> fill (index sl) (write tsl))
                    (slices arr) (slices tarr))
        touchArray arr
        touchArray tarr

    -- | /O(n)/ Sequentially loads vector elements from source to target
    -- in specified range, slice by slice.
    rangeLoadSlicesS
        :: Fill sh e              -- ^ Fill function to work /on slices/
        -> UArray r l sh (v e)    -- ^ Source array of vectors
        -> UArray tr tl sh (v2 e) -- ^ Target array of vectors
        -> sh                     -- ^ Top-left
        -> sh                     -- ^ and bottom-right corners of range to load
        -> IO ()
    rangeLoadSlicesS fill arr tarr start end = do
        force arr
        force tarr
        V.zipWithM_
            (\sl tsl -> rangeLoadS fill sl tsl start end)
            (slices arr) (slices tarr)
        touchArray arr
        touchArray tarr

    {-# INLINE rangeLoadSlicesP #-}
    {-# INLINE rangeLoadSlicesS #-}


-- | /O(n)/ This function simplifies the most common way of loading
-- arrays.
--
-- Instead of
--
-- @
-- mTarget <- 'new' (extent source)
-- 'loadP' 'fill' 'caps' source mTarget
-- target <- 'freeze' mTarget
-- @
--
-- You can write just
--
-- @target <- compute ('loadP' 'fill' 'caps') source@
compute
    :: (USource r l sh a, Manifest tr mtr tl sh b)
    => (UArray r l sh a ->
        UArray mtr tl sh b ->
        IO ())                -- ^ Loading function
    -> UArray r l sh a        -- ^ Source array
    -> IO (UArray tr tl sh b) -- ^ Entirely loaded from the source,
                              -- 'freeze'd manifest target array
{-# INLINE compute #-}
compute load = \arr -> do
    marr <- new (extent arr)
    load arr marr
    freeze marr

-- | Most common parallel use case of 'compute'.
--
-- @dComputeP = 'compute' ('loadP' 'S.fill' 'caps')@
dComputeP
    :: (USource r l sh a, Manifest tr mtr tl sh a,
        Load r l mtr tl sh a)
    => UArray r l sh a 
    -> IO (UArray tr tl sh a)
{-# INLINE dComputeP #-}
dComputeP = compute (loadP fill caps)


-- | Most common sequential use case of 'compute'.
--
-- @dComputeS = 'compute' ('loadS' 'S.fill')@
dComputeS
    :: (USource r l sh a, Manifest tr mtr tl sh a,
        Load r l mtr tl sh a)
    => UArray r l sh a 
    -> IO (UArray tr tl sh a)
{-# INLINE dComputeS #-}
dComputeS = compute (loadS fill)

-- | Determines maximum common range of 2 arrays -
-- 'intersect'ion of their 'extent's.
entire :: (Regular r l sh a, Regular r2 l2 sh b)
       => UArray r l sh a -> UArray r2 l2 sh b -> sh
{-# INLINE entire #-}
entire arr tarr = intersect (vl_2 (extent arr) (extent tarr))

-- | Linear load type index. 'UArray's with 'L' load type index
-- define 'linearIndex' and 'linearWrite' and leave 'index' and 'write'
-- functions defined by default.
data L

instance WorkIndex sh Int => PreferredWorkIndex L sh Int

instance (USource r L sh a, UTarget tr L sh a, WorkIndex sh Int) =>
        Load r L tr L sh a where
    type LoadIndex L L sh = Int

instance Load r L tr L sh a => RangeLoad r L tr L sh a

instance (UVecSource r slr L sh v e, UVecTarget tr tslr L sh v2 e,
          Load slr L tslr L sh e, Dim v ~ Dim v2) =>
        VecLoad r slr L tr tslr L sh v v2 e

instance (VecLoad r slr L tr tslr L sh v v2 e, RangeLoad slr L tslr L sh e) =>
        RangeVecLoad r slr L tr tslr L sh v v2 e


-- | General shape load type index. 'UArray's with 'SH' load type index
-- specialize 'index' and 'write' and leave 'linearIndex' and 'linearWrite'
-- functions defined by default.
-- 
-- Type-level distinction between 'L'inear and 'SH'aped arrays
-- is aimed to avoid integral division operations while looping
-- through composite ('Dim2', 'Dim3') indices.
--
-- Integral division is very expensive operation even on modern CPUs.
data SH

instance Shape sh => PreferredWorkIndex SH sh sh

#define SH_LOAD_INST(l,tl)                                          \
instance (USource r l sh a, UTarget tr tl sh a) =>                  \
        Load r l tr tl sh a where {                                 \
    type LoadIndex l tl sh = sh;                                    \
};                                                                  \
instance Load r l tr tl sh a => RangeLoad r l tr tl sh a;           \
instance (UVecSource r slr l sh v e, UVecTarget tr tslr tl sh v2 e, \
          Load slr l tslr tl sh e, Dim v ~ Dim v2) =>               \
        VecLoad r slr l tr tslr tl sh v v2 e;                       \
instance (VecLoad r slr l tr tslr tl sh v v2 e,                     \
          RangeLoad slr l tslr tl sh e) =>                          \
        RangeVecLoad r slr l tr tslr tl sh v v2 e;                  \

SH_LOAD_INST(SH,L)
SH_LOAD_INST(L,SH)
SH_LOAD_INST(SH,SH)
