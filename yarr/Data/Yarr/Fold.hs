
module Data.Yarr.Fold (
    -- * Aliases for fold functions
    Fold, Foldl, Foldr,

    -- * Fold combinators
    -- | See source of these 4 functions
    -- to construct more similar ones,
    -- if you need.
    reduceL, reduceLeftM,
    reduceR, reduceRightM,

    -- * Fold runners
    runFold, runIFold, runFoldP, runIFoldP,
    runFoldSlicesSeparate, runIFoldSlicesSeparate,
    runFoldSlicesSeparateP, runIFoldSlicesSeparateP,

    -- * Shortcuts
    toList,
) where

import Prelude as P
import Control.Monad as M
import Data.List (groupBy)
import Data.Function (on)

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Eval

import Data.Yarr.Utils.FixedVector as V hiding (toList)
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel


-- | /O(0)/
reduceLeftM
    :: Foldl sh a b     -- ^ 'S.foldl' or curried 'S.unrolledFoldl'
    -> (b -> a -> IO b) -- ^ Monaric left reduce
    -> Fold sh a b      -- ^ Curried fold to be passed to 'runFold' functions.
{-# INLINE reduceLeftM #-}
reduceLeftM foldl rf = foldl (\b _ a -> rf b a)

-- | /O(0)/
reduceL
    :: Foldl sh a b  -- ^ 'S.foldl' or curried 'S.unrolledFoldl'
    -> (b -> a -> b) -- ^ Pure left reduce
    -> Fold sh a b   -- ^ Curried fold to be passed to 'runFold' functions.
{-# INLINE reduceL #-}
reduceL foldl rf = foldl (\b _ a -> return $ rf b a)

-- | /O(0)/
reduceRightM
    :: Foldr sh a b     -- ^ 'S.foldr' or curried 'S.unrolledFoldr'
    -> (a -> b -> IO b) -- ^ Monaric right reduce
    -> Fold sh a b      -- ^ Curried fold to be passed to 'runFold' functions.
{-# INLINE reduceRightM #-}
reduceRightM foldr rf = foldr (\_ a b -> rf a b)

-- | /O(0)/
reduceR
    :: Foldr sh a b  -- ^ 'S.foldr' or curried 'S.unrolledFoldr'
    -> (a -> b -> b) -- ^ Pure right reduce
    -> Fold sh a b   -- ^ Curried fold to be passed to 'runFold' functions.
{-# INLINE reduceR #-}
reduceR foldr rf = foldr (\_ a b -> return $ rf a b)




-- | /O(n)/ Run non-indexed fold.
--
-- Example:
--
-- @'toList' = runFold ('reduceR' 'S.foldr' (:)) (return [])@
runFold
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => Fold i a b       -- ^ Curried folding worker function
    -> IO b             -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> UArray r l sh a  -- ^ Source array 
    -> IO b             -- ^ Fold result
{-# INLINE runFold #-}
runFold = runAnyFold

-- | /O(n)/ Run indexed fold.
--
-- Example:
--
-- @res \<- runIFold ('S.foldl' (\\b i a -> ...)) foldZero sourceArray@
runIFold
    :: USource r l sh a
    => Fold sh a b      -- ^ Curried folding worker function
    -> IO b             -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> UArray r l sh a  -- ^ Source array 
    -> IO b             -- ^ Fold result
{-# INLINE runIFold #-}
runIFold = runAnyFold

runAnyFold
    :: (USource r l sh a, WorkIndex sh i)
    => Fold i a b -> IO b -> UArray r l sh a -> IO b
{-# INLINE runAnyFold #-}
runAnyFold fold mz arr = do
    force arr
    res <- fold mz (gindex arr) zero (gsize arr)
    touchArray arr
    return res


-- | /O(n)/ Run associative non-indexed fold in parallel.
--
-- Example -- associative image histogram filling in the test:
-- <https://github.com/leventov/yarr/blob/master/tests/lum-equalization.hs>
runFoldP
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => Threads          -- ^ Number of threads to parallelize folding on
    -> Fold i a b       -- ^ Curried associative folding worker function
    -> IO b             -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b) -- ^ Associative monadic result joining function
    -> UArray r l sh a  -- ^ Source array
    -> IO b             -- ^ Fold result
{-# INLINE runFoldP #-}
runFoldP = runAnyFoldP

-- | /O(n)/ Run associative indexed fold in parallel.
runIFoldP
    :: USource r l sh a
    => Threads          -- ^ Number of threads to parallelize folding on
    -> Fold sh a b      -- ^ Curried associative folding worker function
    -> IO b             -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b) -- ^ Associative monadic result joining function
    -> UArray r l sh a  -- ^ Source array
    -> IO b             -- ^ Fold result
{-# INLINE runIFoldP #-}
runIFoldP = runAnyFoldP

runAnyFoldP
    :: (USource r l sh a, WorkIndex sh i)
    => Threads -> Fold i a b -> IO b -> (b -> b -> IO b) -> UArray r l sh a -> IO b
{-# INLINE runAnyFoldP #-}
runAnyFoldP threads fold mz join arr = do
    force arr
    ts <- threads
    (r:rs) <- parallel ts $
                makeFork ts zero (gsize arr) (fold mz (gindex arr))
    touchArray arr

    M.foldM join r rs


-- | /O(n)/ Run non-indexed fold over each slice of array of vectors.
runFoldSlicesSeparate
    :: (UVecSource r slr l sh v e, PreferredWorkIndex l sh i)
    => Fold i e b             -- ^ Curried folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runFoldSlicesSeparate #-}
runFoldSlicesSeparate = runAnyFoldSlicesSeparate

-- | /O(n)/ Run indexed fold over each slice of array of vectors.
runIFoldSlicesSeparate
    :: UVecSource r slr l sh v e
    => Fold sh e b            -- ^ Curried folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runIFoldSlicesSeparate #-}
runIFoldSlicesSeparate = runAnyFoldSlicesSeparate

runAnyFoldSlicesSeparate
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => Fold i e b -> IO b -> UArray r l sh (v e) -> IO (VecList (Dim v) b)
{-# INLINE runAnyFoldSlicesSeparate #-}
runAnyFoldSlicesSeparate fold mz arr = do
    force arr
    rs <- V.mapM (\sl -> runAnyFold fold mz sl) (slices arr)
    touchArray arr
    return rs


-- | /O(n)/ Run associative non-indexed fold over slices of array of vectors in parallel.
runFoldSlicesSeparateP
    :: (UVecSource r slr l sh v e, PreferredWorkIndex l sh i)
    => Threads                -- ^ Number of threads to parallelize folding on
    -> Fold i e b             -- ^ Curried associative folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b)       -- ^ Associative monadic result joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runFoldSlicesSeparateP #-}
runFoldSlicesSeparateP = runAnyFoldSlicesSeparateP

-- | /O(n)/ Run associative indexed fold over slices of array of vectors in parallel.
runIFoldSlicesSeparateP
    :: UVecSource r slr l sh v e
    => Threads                -- ^ Number of threads to parallelize folding on
    -> Fold sh e b            -- ^ Curried associative folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b)       -- ^ Associative monadic result joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runIFoldSlicesSeparateP #-}
runIFoldSlicesSeparateP = runAnyFoldSlicesSeparateP

runAnyFoldSlicesSeparateP
    :: (UVecSource r slr l sh v e, WorkIndex sh i)
    => Threads                -- ^ Number of threads to parallelize folding on
    -> Fold i e b             -- ^ Curried associative folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b)       -- ^ Associative monadic result joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runAnyFoldSlicesSeparateP #-}
runAnyFoldSlicesSeparateP threads fold mz join arr = do
    force arr
    let sls = slices arr
    V.mapM force sls

    ts <- threads
    trs <- parallel ts $
            makeForkSlicesOnce
                ts
                (V.replicate (zero, gsize arr))
                (V.map (\sl -> fold mz (gindex sl)) sls)
    touchArray arr

    let rsBySlices = P.map (P.map snd) $ groupBy ((==) `on` fst) $ concat trs
    rs <- M.mapM (\(r:rs) -> M.foldM join r rs) rsBySlices
    return (VecList rs)


-- | /O(n)/ Covert array to list.
toList
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => UArray r l sh a
    -> IO [a]
toList = runFold (reduceR S.foldr (:)) (return [])
