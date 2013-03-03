
module Data.Yarr.Walk (
    -- * Fold combinators
    -- | See source of these 4 functions
    -- to construct more similar ones,
    -- if you need.
    reduceL, reduceLeftM,
    reduceR, reduceRightM,

    -- * Combinators to walk with mutable state
    -- | Added specially to improve performance
    -- of tasks like histogram filling.
    --
    -- Unfortunately, GHC doesn't figure that folding state
    -- isn't changed as ADT in such cases and doesn't lift
    -- it's evaluation higher from folding routine.
    mutate, imutate,

    -- * Walk runners
    walk, iwalk, rangeWalk,
    walkP, iwalkP, rangeWalkP,
    walkSlicesSeparate, iwalkSlicesSeparate, rangeWalkSlicesSeparate,
    walkSlicesSeparateP, iwalkSlicesSeparateP, rangeWalkSlicesSeparateP,

    -- * Aliases for walk types
    StatefulWalk, Foldl, Foldr,
) where

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Eval

import Data.Yarr.Walk.Internal


-- | /O(0)/
reduceLeftM
    :: Foldl i a b        -- ^ 'S.foldl' or curried 'S.unrolledFoldl'
    -> (b -> a -> IO b)   -- ^ Monadic left reduce
    -> StatefulWalk i a b -- ^ Result stateful walk to be passed
                          -- to walk runners
{-# INLINE reduceLeftM #-}
reduceLeftM foldl rf = foldl (\b _ a -> rf b a)

-- | /O(0)/
reduceL
    :: Foldl i a b        -- ^ 'S.foldl' or curried 'S.unrolledFoldl'
    -> (b -> a -> b)      -- ^ Pure left reduce
    -> StatefulWalk i a b -- ^ Result stateful walk to be passed
                          -- to walk runners
{-# INLINE reduceL #-}
reduceL foldl rf = foldl (\b _ a -> return $ rf b a)

-- | /O(0)/
reduceRightM
    :: Foldr i a b         -- ^ 'S.foldr' or curried 'S.unrolledFoldr'
    -> (a -> b -> IO b)    -- ^ Monadic right reduce
    -> StatefulWalk i a b  -- ^ Result stateful walk to be passed
                           -- to walk runners
{-# INLINE reduceRightM #-}
reduceRightM foldr rf = foldr (\_ a b -> rf a b)

-- | /O(0)/
reduceR
    :: Foldr i a b        -- ^ 'S.foldr' or curried 'S.unrolledFoldr'
    -> (a -> b -> b)      -- ^ Pure right reduce
    -> StatefulWalk i a b -- ^ Result stateful walk to be passed
                          -- to walk runners
{-# INLINE reduceR #-}
reduceR foldr rf = foldr (\_ a b -> return $ rf a b)


-- | /O(0)/
mutate
    :: Fill i a           -- ^ 'S.fill' or curried 'S.unrolledFill'.
                          -- If mutating is associative,
                          -- 'S.dim2BlockFill' is also acceptable.
    -> (s -> a -> IO ())  -- ^ (state -> array element -> (state has changed))
                          -- -- State mutating function
    -> StatefulWalk i a s -- ^ Result stateful walk to be passed
                          -- to walk runners
{-# INLINE mutate #-}
mutate fill mf = imutate fill (\s i -> mf s)

-- | /O(0)/ Version of 'mutate', accepts mutating function
-- which additionaly accepts array index.
imutate
    :: Fill i a               -- ^ 'S.fill' or curried 'S.unrolledFill'.
                              -- If mutating is associative,
                              -- 'S.dim2BlockFill' is also acceptable.
    -> (s -> i -> a -> IO ()) -- ^ Indexed state mutating function
    -> StatefulWalk i a s     -- ^ Result stateful walk to be passed
                              -- to walk runners
{-# INLINE imutate #-}
imutate fill imf ms index start end = do
    s <- ms
    fill index (imf s) start end
    return s



-- | /O(n)/ Walk with state,
-- with non-indexed function ('reduceL' group of fold combinators, 'mutate').
--
-- Example:
--
-- @'Data.Yarr.IO.List.toList' = walk ('reduceR' 'S.foldr' (:)) (return [])@
walk
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => StatefulWalk i a s -- ^ Stateful walking function
    -> IO s               -- ^ Monadic initial state (fold zero).
                          -- Wrap pure state in 'return'.
    -> UArray r l sh a    -- ^ Source array
    -> IO s               -- ^ Final state (fold result)
{-# INLINE walk #-}
walk = anyWalk

-- | /O(n)/ Walk with state,
-- with indexed function ('S.foldl', 'S.foldr', 'imutate', etc).
--
-- Example:
--
-- @res \<- iwalk ('S.foldl' (\\s i a -> ...)) foldZero sourceArray@
iwalk
    :: USource r l sh a
    => StatefulWalk sh a s -- ^ Stateful walking function
    -> IO s                -- ^ Monadic initial state (fold zero).
                           -- Wrap pure state in 'return'.
    -> UArray r l sh a     -- ^ Source array
    -> IO s                -- ^ Final state (fold result)
{-# INLINE iwalk #-}
iwalk = anyWalk

-- | /O(n)/ Walk with state, in specified range of indices.
rangeWalk
    :: USource r l sh a
    => StatefulWalk sh a s -- ^ Stateful walking function
    -> IO s                -- ^ Monadic initial state (fold zero).
                           -- Wrap pure state in 'return'.
    -> UArray r l sh a     -- ^ Source array
    -> sh                  -- ^ Top-left
    -> sh                  -- ^ and bottom-right corners of range to walk in
    -> IO s                -- ^ Final state (fold result)
{-# INLINE rangeWalk #-}
rangeWalk = anyRangeWalk


-- | /O(n)/ Run associative non-indexed stateful walk, in parallel.
--
-- Example -- associative image histogram filling in the test:
-- <https://github.com/leventov/yarr/blob/master/tests/lum-equalization.hs>
walkP
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => Threads            -- ^ Number of threads to parallelize walk on
    -> StatefulWalk i a s -- ^ Associative stateful walking function
    -> IO s               -- ^ Monadic zero state.
                          -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)   -- ^ Associative monadic state joining function
    -> UArray r l sh a    -- ^ Source array
    -> IO s               -- ^ Gathered state (fold result)
{-# INLINE walkP #-}
walkP = anyWalkP

-- | /O(n)/ Run associative indexed stateful walk, in parallel.
iwalkP
    :: USource r l sh a
    => Threads             -- ^ Number of threads to parallelize walk on
    -> StatefulWalk sh a s -- ^ Associative stateful walking function
    -> IO s                -- ^ Monadic zero state.
                           -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)    -- ^ Associative monadic state joining function
    -> UArray r l sh a     -- ^ Source array
    -> IO s                -- ^ Gathered state (fold result)
{-# INLINE iwalkP #-}
iwalkP = anyWalkP

-- | /O(n)/ Run associative stateful walk in specified range, in parallel.
rangeWalkP
    :: USource r l sh a
    => Threads             -- ^ Number of threads to parallelize walk on
    -> StatefulWalk sh a s -- ^ Associative stateful walking function
    -> IO s                -- ^ Monadic zero state.
                           -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)    -- ^ Associative monadic state joining function
    -> UArray r l sh a     -- ^ Source array
    -> sh                  -- ^ Top-left
    -> sh                  -- ^ and bottom-right corners of range to walk in
    -> IO s                -- ^ Gathered state (fold result)
{-# INLINE rangeWalkP #-}
rangeWalkP = anyRangeWalkP


-- | /O(n)/ Walk with state, with non-indexed function,
-- over each slice of array of vectors.
walkSlicesSeparate
    :: (UVecSource r slr l sh v e, PreferredWorkIndex l sh i)
    => StatefulWalk i e s     -- ^ Stateful slice-wise walking function
    -> IO s                   -- ^ Monadic initial state (fold zero).
                              -- Wrap pure state in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of final states (fold results)
{-# INLINE walkSlicesSeparate #-}
walkSlicesSeparate = anyWalkSlicesSeparate

-- | /O(n)/ Walk with state, with indexed function,
-- over each slice of array of vectors.
iwalkSlicesSeparate
    :: UVecSource r slr l sh v e
    => StatefulWalk sh e s    -- ^ Stateful slice-wise walking function
    -> IO s                   -- ^ Monadic initial state (fold zero).
                              -- Wrap pure state in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of final states (fold results)
{-# INLINE iwalkSlicesSeparate #-}
iwalkSlicesSeparate = anyWalkSlicesSeparate

-- | /O(n)/ Walk with state, in specified range of indices,
-- over each slice of array of vectors.
rangeWalkSlicesSeparate
    :: UVecSource r slr l sh v e
    => StatefulWalk sh e s    -- ^ Stateful slice-wise walking function
    -> IO s                   -- ^ Monadic initial state (fold zero).
                              -- Wrap pure state in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> sh                     -- ^ Top-left
    -> sh                     -- ^ and bottom-right corners of range to walk in
    -> IO (VecList (Dim v) s) -- ^ Vector of final states (fold results)
{-# INLINE rangeWalkSlicesSeparate #-}
rangeWalkSlicesSeparate = anyRangeWalkSlicesSeparate


-- | /O(n)/ Run associative non-indexed stateful walk
-- over slices of array of vectors, in parallel.
walkSlicesSeparateP
    :: (UVecSource r slr l sh v e, PreferredWorkIndex l sh i)
    => Threads                -- ^ Number of threads to parallelize walk on
    -> StatefulWalk i e s     -- ^ Stateful slice-wise walking function
    -> IO s                   -- ^ Monadic zero state.
                              -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)       -- ^ Associative monadic state joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of gathered per slice results
{-# INLINE walkSlicesSeparateP #-}
walkSlicesSeparateP = anyWalkSlicesSeparateP

-- | /O(n)/ Run associative indexed stateful walk
-- over slices of array of vectors, in parallel.
iwalkSlicesSeparateP
    :: UVecSource r slr l sh v e
    => Threads                -- ^ Number of threads to parallelize walk on
    -> StatefulWalk sh e s    -- ^ Stateful slice-wise walking function
    -> IO s                   -- ^ Monadic zero state.
                              -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)       -- ^ Associative monadic state joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of gathered per slice results
{-# INLINE iwalkSlicesSeparateP #-}
iwalkSlicesSeparateP = anyWalkSlicesSeparateP

-- | /O(n)/ Run associative stateful walk in specified range,
-- over slices of array of vectors, in parallel.
rangeWalkSlicesSeparateP
    :: UVecSource r slr l sh v e
    => Threads                -- ^ Number of threads to parallelize walk on
    -> StatefulWalk sh e s    -- ^ Stateful slice-wise walking function
    -> IO s                   -- ^ Monadic zero state.
                              -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)       -- ^ Associative monadic state joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> sh                     -- ^ Top-left
    -> sh                     -- ^ and bottom-right corners of range to walk in
    -> IO (VecList (Dim v) s) -- ^ Vector of gathered per slice results
{-# INLINE rangeWalkSlicesSeparateP #-}
rangeWalkSlicesSeparateP = anyRangeWalkSlicesSeparateP
