
module Data.Yarr.Utils.Storable where

import Foreign
import Data.Yarr.Utils.FixedVector as V

instance (Storable e, Vector v e) => Storable (v e) where
    sizeOf _ =
        let esize = sizeOf (undefined :: e)
            n = arity (undefined :: (Dim v))
        in n * esize

    alignment _ = alignment (undefined :: e)

    peek ptr =
        let eptr = castPtr ptr
        in V.generateM (\i -> peekElemOff eptr i)

    poke ptr v =
        let eptr = castPtr ptr
        in imapM_ (\i e -> pokeElemOff eptr i e) v

    {-# INLINE sizeOf #-}
    {-# INLINE alignment #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}