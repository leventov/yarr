{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}

module Debug.Yarr (
    -- * @Checked@ meta repr
    CHK,
    -- | There is also @Checked@ 'UArray' family constructor,
    -- which isn't presented in the docs because Haddock
    -- doesn't support associated family constructors.
    --
    -- See source of "Yarr.Debug" module.
    UArray(..),

    -- * Fun
    yarr, yerr
) where

import System.IO
import Text.Printf

import Data.Yarr.Base hiding (fmap)
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V hiding ( zero, index )

-- | Yarr something to stderr.
yarr :: String -> IO ()
yarr smth = hPutStrLn stderr ("Yarr! " ++ smth)

-- | Yarr something as 'error' message.
yerr :: String -> a
yerr msg = error ("Yarr! " ++ msg)

data CHK r

instance Regular r l sh a => Regular (CHK r) l sh a where
    newtype UArray (CHK r) l sh a = Checked { unchecked :: UArray r l sh a }

    extent = extent . unchecked
    touchArray = touchArray . unchecked

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}

instance NFData (UArray r l sh a) => NFData (UArray (CHK r) l sh a) where
    rnf = rnf . unchecked
    {-# INLINE rnf #-}

instance VecRegular r slr l sh v e =>
        VecRegular (CHK r) (CHK slr) l sh v e where
    slices = V.map Checked . slices . unchecked
    {-# INLINE slices #-}


instance USource r l sh a => USource (CHK r) l sh a where
    index (Checked arr) sh =
        let ext = extent arr
        in if not (insideBlock (zero, ext) sh)
            then yerr $ printf "Index %s is out of extent - %s"
                               (show sh) (show ext)
            else index arr sh

    linearIndex (Checked arr) i =
        let sz = size (extent arr)
        in if not (insideBlock (0, sz) i)
            then yerr $ printf "Linear index %d is out of size - %d" i sz
            else linearIndex arr i

    {-# INLINE index #-}
    {-# INLINE linearIndex #-}

instance UVecSource r slr l sh v e =>
        UVecSource (CHK r) (CHK slr) l sh v e where


instance UTarget tr tl sh a => UTarget (CHK tr) tl sh a where
    write (Checked arr) sh =
        let ext = extent arr
        in if not (insideBlock (zero, ext) sh)
            then yerr $ printf "Writing: index %s is out of extent - %s"
                               (show sh) (show ext)
            else write arr sh

    linearWrite (Checked arr) i =
        let sz = size (extent arr)
        in if not (insideBlock (0, sz) i)
            then yerr $ printf "Writing: linear index %d is out of size - %d"
                               i sz
            else linearWrite arr i
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance Manifest r mr l sh a => Manifest (CHK r) (CHK mr) l sh a where
    new sh = fmap Checked (new sh)
    freeze (Checked marr) = fmap Checked (freeze marr)
    thaw (Checked arr) = fmap Checked (thaw arr)
    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}

instance UVecTarget tr tslr l sh v e => UVecTarget (CHK tr) (CHK tslr) l sh v e
