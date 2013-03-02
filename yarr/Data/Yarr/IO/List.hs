
module Data.Yarr.IO.List where

import Control.Monad

import Data.Yarr.Base
import Data.Yarr.Shape as S
import Data.Yarr.Eval
import Data.Yarr.Walk

import Debug.Yarr

-- | /O(n)/ Covert array to flat list.
-- Multidimentional arrays are flatten in column-major order:
-- 
-- \[(elem at (0, .., 0, 1)), (elem at (0, .., 0, 2)), ...\]
toList
    :: (USource r l sh a, PreferredWorkIndex l sh i)
    => UArray r l sh a -> IO [a]
{-# INLINE toList #-}
toList = walk (reduceR S.foldr (:)) (return [])

-- | /O(n)/ Loads manifest array into memory, with elements
-- from flatten list.
--
-- Use this function in the last resort, there are plenty of
-- methods to 'Load' array, from 'Data.Yarr.D'elayed array for example.
fromList
    :: Manifest r mr l sh a
    => sh                   -- ^ Extent of array
    -> [a]                  -- ^ Flatten elements
    -> IO (UArray r l sh a) -- ^ Result manifest array
{-# INLINE fromList #-}
fromList sh xs =
    if (length xs) /= (size sh)
        then yerr "fromList: list length doesn't correspond size of array shape"
        else do
            arr <- new sh
            zipWithM_ (linearWrite arr) [0..] xs
            freeze arr