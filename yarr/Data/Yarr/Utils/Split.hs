
module Data.Yarr.Utils.Split where

makeSplitIndex
    :: Int
    -> Int -> Int
    -> (Int -> Int)
{-# INLINE makeSplitIndex #-}
makeSplitIndex chunks start end =
    let !len = end - start
    in if len < chunks
            then \c -> start + (min c len)
            else let (chunkLen, chunkLeftover) = len `quotRem` chunks
                 in \c -> if c < chunkLeftover
                        then start + c * (chunkLen + 1)
                        else start + c * chunkLen + chunkLeftover

evenChunks :: [a] -> Int -> [[a]]
{-# INLINE evenChunks #-}
evenChunks xs n =
    let len = length xs
        {-# INLINE splitIndex #-}
        splitIndex = makeSplitIndex n 0 len
        chunk i =
            let s = splitIndex i
                e = splitIndex (i + 1)
            in take (e - s) (drop s xs)
    in map chunk [0..n-1]