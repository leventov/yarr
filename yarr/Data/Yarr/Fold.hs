
module Data.Yarr.Fold (
    -- * Fold support
    Fold,
    reduceL, reduceLeftM,
    reduceR, reduceRightM,

    -- * Fold runners
    runFold, runFoldP,
    runFoldSlicesSeparate, runFoldSlicesSeparateP,

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
import Data.Yarr.Convolution

import Data.Yarr.Utils.FixedVector as V hiding (toList)
import Data.Yarr.Utils.Fork
import Data.Yarr.Utils.Parallel


class Shape fsh => Reduce l ash fsh | l ash -> fsh where
    getIndex :: USource r l ash a => UArray r l ash a -> (fsh -> IO a)
    getSize :: USource r l ash a => UArray r l ash a -> fsh

#define SH_GET_INDEX(l,sh)      \
instance Reduce l sh sh where { \
    getIndex = index;           \
    getSize = extent;           \
    {-# INLINE getIndex #-};    \
    {-# INLINE getSize #-};     \
}

SH_GET_INDEX(SH, Dim1)
SH_GET_INDEX(SH, Dim2)
SH_GET_INDEX(SH, Dim3)

SH_GET_INDEX(CVL, Dim1)
SH_GET_INDEX(CVL, Dim2)
SH_GET_INDEX(CVL, Dim3)


#define LINEAR_GET_INDEX(l,sh)   \
instance Reduce l sh Int where { \
    getIndex = linearIndex;      \
    getSize = size . extent;     \
    {-# INLINE getIndex #-};     \
    {-# INLINE getSize #-};      \
}

LINEAR_GET_INDEX(L, Dim1)
LINEAR_GET_INDEX(L, Dim2)
LINEAR_GET_INDEX(L, Dim3)


-- | Curried 'Foldl' or 'Foldr'.
-- Generalizes both partially applied left and right folds.
--
-- See source of following 4 functions to construct more similar ones,
-- if you need.
type Fold sh a b = 
       IO b         -- ^ Zero
    -> (sh -> IO a) -- ^ Get
    -> sh           -- ^ Start
    -> sh           -- ^ End
    -> IO b         -- ^ Result

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


-- | /O(n)/
--
-- Example:
--
-- @'toList' = runFold ('reduceR' 'S.foldr' (:)) (return [])@
runFold
    :: (USource r l sh a, Reduce l sh fsh)
    => Fold fsh a b     -- ^ Curried folding worker function
    -> IO b             -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> UArray r l sh a  -- ^ Source array 
    -> IO b             -- ^ Fold result
{-# INLINE runFold #-}
runFold fold mz arr = do
    force arr
    res <- fold mz (getIndex arr) zero (getSize arr)
    touchArray arr
    return res

-- | /O(n)/ Run associative fold in parallel.
--
-- Example -- associative image histogram filling in the test:
-- <https://github.com/leventov/yarr/blob/master/tests/lum-equalization.hs>
runFoldP
    :: (USource r l sh a, Reduce l sh fsh)
    => Threads          -- ^ Number of threads to parallelize folding on
    -> Fold fsh a b     -- ^ Curried folding worker function
    -> IO b             -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b) -- ^ Associative monadic result joining function
    -> UArray r l sh a  -- ^ Source array
    -> IO b             -- ^ Fold result
{-# INLINE runFoldP #-}
runFoldP threads fold mz join arr = do
    force arr
    ts <- threads
    (r:rs) <- parallel ts $
                makeFork ts zero (getSize arr) (fold mz (getIndex arr))
    touchArray arr

    M.foldM join r rs

-- | /O(n)/ 
runFoldSlicesSeparate
    :: (UVecSource r slr l sh v e, Reduce l sh fsh)
    => Fold fsh e b           -- ^ Curried folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runFoldSlicesSeparate #-}
runFoldSlicesSeparate fold mz arr =
    V.mapM (\sl -> runFold fold mz sl) (slices arr)

-- | /O(n)/ Run associative fold over slices of array of vectors in parallel.
runFoldSlicesSeparateP
    :: (UVecSource r slr l sh v e, Reduce l sh fsh)
    => Threads                -- ^ Number of threads to parallelize folding on
    -> Fold fsh e b           -- ^ Curried folding function to work on slices
    -> IO b                   -- ^ Monadic fold zero. Wrap pure zero in 'return'.
    -> (b -> b -> IO b)       -- ^ Associative monadic result joining function
    -> UArray r l sh (v e)    -- ^ Source array of vectors
    -> IO (VecList (Dim v) b) -- ^ Vector of fold results
{-# INLINE runFoldSlicesSeparateP #-}
runFoldSlicesSeparateP threads fold mz join arr = do
    force arr
    ts <- threads
    trs <- parallel ts $
            makeForkSlicesOnce
                ts
                (V.replicate (zero, getSize arr))
                (V.map (\sl -> fold mz (getIndex sl)) (slices arr))
    touchArray arr

    let rsBySlices = P.map (P.map snd) $ groupBy ((==) `on` fst) $ concat trs
    rs <- M.mapM (\(r:rs) -> M.foldM join r rs) rsBySlices
    return (VecList rs)

-- | /O(n)/ Covert array to list.
toList
    :: (USource r l sh a, Reduce l sh fsh)
    => UArray r l sh a
    -> IO [a]
toList = runFold (reduceR S.foldr (:)) (return [])
