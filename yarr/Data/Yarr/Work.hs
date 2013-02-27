
module Data.Yarr.Work (
    -- * Fold combinators
    -- | See source of these 4 functions
    -- to construct more similar ones,
    -- if you need.
    reduceL, reduceLeftM,
    reduceR, reduceRightM,

    -- * Combinators to work with mutable state
    -- | Added specially to improve performance
    -- of tasks like histogram filling.
    --
    -- Unfortunately, GHC doesn't figure that folding state
    -- isn't changed as ADT in such cases and doesn't lift
    -- it's evaluation higher from folding routine.
    mutate, imutate,

    -- * Work runners
    work, iwork, rangeWork,
    workP, iworkP, rangeWorkP,
    workOnSlicesSeparate, iworkOnSlicesSeparate, rangeWorkOnSlicesSeparate,
    workOnSlicesSeparateP, iworkOnSlicesSeparateP, rangeWorkOnSlicesSeparateP,

    -- * Aliases for work types
    StatefulWork, Foldl, Foldr,
) where

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Eval

import Data.Yarr.Work.Internal


-- | /O(0)/
reduceLeftM
    :: Foldl i a b        -- ^ 'S.foldl' or curried 'S.unrolledFoldl'
    -> (b -> a -> IO b)   -- ^ Monadic left reduce
    -> StatefulWork i a b -- ^ Result stateful work to be passed
                          -- to work runners
{-# INLINE reduceLeftM #-}
reduceLeftM foldl rf = foldl (\b _ a -> rf b a)

-- | /O(0)/
reduceL
    :: Foldl i a b        -- ^ 'S.foldl' or curried 'S.unrolledFoldl'
    -> (b -> a -> b)      -- ^ Pure left reduce
    -> StatefulWork i a b -- ^ Result stateful work to be passed
                          -- to work runners
{-# INLINE reduceL #-}
reduceL foldl rf = foldl (\b _ a -> return $ rf b a)

-- | /O(0)/
reduceRightM
    :: Foldr i a b         -- ^ 'S.foldr' or curried 'S.unrolledFoldr'
    -> (a -> b -> IO b)    -- ^ Monadic right reduce
    -> StatefulWork i a b  -- ^ Result stateful work to be passed
                           -- to work runners
{-# INLINE reduceRightM #-}
reduceRightM foldr rf = foldr (\_ a b -> rf a b)

-- | /O(0)/
reduceR
    :: Foldr i a b        -- ^ 'S.foldr' or curried 'S.unrolledFoldr'
    -> (a -> b -> b)      -- ^ Pure right reduce
    -> StatefulWork i a b -- ^ Result stateful work to be passed
                          -- to work runners
{-# INLINE reduceR #-}
reduceR foldr rf = foldr (\_ a b -> return $ rf a b)


-- | /O(0)/
mutate
    :: Fill i a           -- ^ 'S.fill' or curried 'S.unrolledFill'.
                          -- If mutating is associative,
                          -- 'S.dim2BlockFill' is also acceptable.
    -> (s -> a -> IO ())  -- ^ (state -> array element -> (state has changed))
                          -- -- State mutating function
    -> StatefulWork i a s -- ^ Result stateful work to be passed
                          -- to work runners
{-# INLINE mutate #-}
mutate fill mf = imutate fill (\s i -> mf s)

-- | /O(0)/ Version of 'mutate', accepts mutating function
-- which additionaly accepts array index.
imutate
    :: Fill i a               -- ^ 'S.fill' or curried 'S.unrolledFill'.
                              -- If mutating is associative,
                              -- 'S.dim2BlockFill' is also acceptable.
    -> (s -> i -> a -> IO ()) -- ^ Indexed state mutating function
    -> StatefulWork i a s     -- ^ Result stateful work to be passed
                              -- to work runners
{-# INLINE imutate #-}
imutate fill imf ms index start end = do
    s <- ms
    fill index (imf s) start end
    return s



-- | /O(n)/ Run non-indexed stateful work.
--
-- Example:
--
-- @'Data.Yarr.IO.List.toList' = work ('reduceR' 'S.foldr' (:)) (return [])@
work
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => StatefulWork i a s -- ^ Stateful working function
    -> IO s               -- ^ Monadic initial state (fold zero).
                          -- Wrap pure state in 'return'.
    -> UArray r l sh a    -- ^ Source array
    -> IO s               -- ^ Final state (fold result)
{-# INLINE work #-}
work = anyWork

-- | /O(n)/ Run indexed stateful work.
--
-- Example:
--
-- @res \<- iwork ('S.foldl' (\\s i a -> ...)) foldZero sourceArray@
iwork
    :: USource r l sh a
    => StatefulWork sh a s -- ^ Stateful working function
    -> IO s                -- ^ Monadic initial state (fold zero).
                           -- Wrap pure state in 'return'.
    -> UArray r l sh a     -- ^ Source array
    -> IO s                -- ^ Final state (fold result)
{-# INLINE iwork #-}
iwork = anyWork

-- | /O(n)/ Run stateful work in specified range of indices.
rangeWork
    :: USource r l sh a
    => StatefulWork sh a s -- ^ Stateful working function
    -> IO s                -- ^ Monadic initial state (fold zero).
                           -- Wrap pure state in 'return'.
    -> UArray r l sh a     -- ^ Source array
    -> sh                  -- ^ Top-left
    -> sh                  -- ^ and bottom-right corners of range to work in
    -> IO s                -- ^ Final state (fold result)
{-# INLINE rangeWork #-}
rangeWork = anyRangeWork


-- | /O(n)/ Run associative non-indexed stateful work in parallel.
--
-- Example -- associative image histogram filling in the test:
-- <https://github.com/leventov/yarr/blob/master/tests/lum-equalization.hs>
workP
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => Threads            -- ^ Number of threads to parallelize work on
    -> StatefulWork i a s -- ^ Associative stateful working function
    -> IO s               -- ^ Monadic zero state.
                          -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)   -- ^ Associative monadic state joining function
    -> UArray r l sh a    -- ^ Source array
    -> IO s               -- ^ Gathered state (fold result)
{-# INLINE workP #-}
workP = anyWorkP

-- | /O(n)/ Run associative indexed stateful work in parallel.
iworkP
    :: USource r l sh a
    => Threads             -- ^ Number of threads to parallelize work on
    -> StatefulWork sh a s -- ^ Associative stateful working function
    -> IO s                -- ^ Monadic zero state.
                           -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)    -- ^ Associative monadic state joining function
    -> UArray r l sh a     -- ^ Source array
    -> IO s                -- ^ Gathered state (fold result)
{-# INLINE iworkP #-}
iworkP = anyWorkP

-- | /O(n)/ Run associative stateful work in specified range in parallel.
rangeWorkP
    :: USource r l sh a
    => Threads             -- ^ Number of threads to parallelize work on
    -> StatefulWork sh a s -- ^ Associative stateful working function
    -> IO s                -- ^ Monadic zero state.
                           -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)    -- ^ Associative monadic state joining function
    -> UArray r l sh a     -- ^ Source array
    -> sh                  -- ^ Top-left
    -> sh                  -- ^ and bottom-right corners of range to work in
    -> IO s                -- ^ Gathered state (fold result)
{-# INLINE rangeWorkP #-}
rangeWorkP = anyRangeWorkP


-- | /O(n)/ Run non-indexed stateful work over each slice of array of vectors.
workOnSlicesSeparate
    :: (UVecSource r slr l sh v e, PreferredWorkIndex l sh i)
    => StatefulWork i e s     -- ^ Stateful slice-wise working function
    -> IO s                   -- ^ Monadic initial state (fold zero).
                              -- Wrap pure state in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of final states (fold results)
{-# INLINE workOnSlicesSeparate #-}
workOnSlicesSeparate = anyWorkOnSlicesSeparate

-- | /O(n)/ Run indexed stateful work over each slice of array of vectors.
iworkOnSlicesSeparate
    :: UVecSource r slr l sh v e
    => StatefulWork sh e s    -- ^ Stateful slice-wise working function
    -> IO s                   -- ^ Monadic initial state (fold zero).
                              -- Wrap pure state in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of final states (fold results)
{-# INLINE iworkOnSlicesSeparate #-}
iworkOnSlicesSeparate = anyWorkOnSlicesSeparate

-- | /O(n)/ Run stateful work in specified range
-- over each slice of array of vectors.
rangeWorkOnSlicesSeparate
    :: UVecSource r slr l sh v e
    => StatefulWork sh e s    -- ^ Stateful slice-wise working function
    -> IO s                   -- ^ Monadic initial state (fold zero).
                              -- Wrap pure state in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> sh                     -- ^ Top-left
    -> sh                     -- ^ and bottom-right corners of range to work in
    -> IO (VecList (Dim v) s) -- ^ Vector of final states (fold results)
{-# INLINE rangeWorkOnSlicesSeparate #-}
rangeWorkOnSlicesSeparate = anyRangeWorkOnSlicesSeparate


-- | /O(n)/ Run associative non-indexed stateful work
-- over slices of array of vectors in parallel.
workOnSlicesSeparateP
    :: (UVecSource r slr l sh v e, PreferredWorkIndex l sh i)
    => Threads                -- ^ Number of threads to parallelize work on
    -> StatefulWork i e s     -- ^ Stateful slice-wise working function
    -> IO s                   -- ^ Monadic zero state.
                              -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)       -- ^ Associative monadic state joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of gathered per slice results
{-# INLINE workOnSlicesSeparateP #-}
workOnSlicesSeparateP = anyWorkOnSlicesSeparateP

-- | /O(n)/ Run associative indexed stateful work
-- over slices of array of vectors in parallel.
iworkOnSlicesSeparateP
    :: UVecSource r slr l sh v e
    => Threads                -- ^ Number of threads to parallelize work on
    -> StatefulWork sh e s    -- ^ Stateful slice-wise working function
    -> IO s                   -- ^ Monadic zero state.
                              -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)       -- ^ Associative monadic state joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) s) -- ^ Vector of gathered per slice results
{-# INLINE iworkOnSlicesSeparateP #-}
iworkOnSlicesSeparateP = anyWorkOnSlicesSeparateP

-- | /O(n)/ Run associative stateful work in specified range
-- over slices of array of vectors in parallel.
rangeWorkOnSlicesSeparateP
    :: UVecSource r slr l sh v e
    => Threads                -- ^ Number of threads to parallelize work on
    -> StatefulWork sh e s    -- ^ Stateful slice-wise working function
    -> IO s                   -- ^ Monadic zero state.
                              -- Wrap pure state in 'return'.
    -> (s -> s -> IO s)       -- ^ Associative monadic state joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> sh                     -- ^ Top-left
    -> sh                     -- ^ and bottom-right corners of range to work in
    -> IO (VecList (Dim v) s) -- ^ Vector of gathered per slice results
{-# INLINE rangeWorkOnSlicesSeparateP #-}
rangeWorkOnSlicesSeparateP = anyRangeWorkOnSlicesSeparateP
