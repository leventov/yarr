
module Data.Yarr.Repr.Checked where

import Text.Printf

import Data.Yarr.Base hiding (fmap)
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


data CHK r

instance Regular r sh a => Regular (CHK r) sh a where
    newtype UArray (CHK r) sh a = Checked { unchecked :: UArray r sh a }

    extent = extent . unchecked
    shapeIndexingPreferred = shapeIndexingPreferred . unchecked
    touch = touch . unchecked

    {-# INLINE extent #-}
    {-# INLINE shapeIndexingPreferred #-}
    {-# INLINE touch #-}

instance NFData (UArray r sh a) => NFData (UArray (CHK r) sh a) where
    rnf = rnf . unchecked
    {-# INLINE rnf #-}

instance VecRegular r sh slr v e => VecRegular (CHK r) sh (CHK slr) v e where
    slices = V.map Checked . slices . unchecked
    {-# INLINE slices #-}

instance USource r sh a => USource (CHK r) sh a where
    index (Checked arr) sh =
        let ext = extent arr
        in if not (insideBlock (zero, ext) sh)
            then error $ printf
                            "Yarr! Index %s is out of extent - %s"
                            (show sh) (show ext)
            else index arr sh

    linearIndex (Checked arr) i =
        let sz = size (extent arr)
        in if not (insideBlock (0, sz) i)
            then error $ printf "Yarr! Linear index %d is out of size - %d" i sz
            else linearIndex arr i


    rangeLoadP threads (Checked arr) = rangeLoadP threads arr
    linearLoadP threads (Checked arr) = linearLoadP threads arr
    rangeLoadS (Checked arr) = rangeLoadS arr
    linearLoadS (Checked arr) = linearLoadS arr

    {-# INLINE index #-}
    {-# INLINE linearIndex #-}
    {-# INLINE rangeLoadP #-}
    {-# INLINE linearLoadP #-}
    {-# INLINE rangeLoadS #-}
    {-# INLINE linearLoadS #-}

instance UVecSource r sh slr v e => UVecSource (CHK r) sh (CHK slr) v e where
    rangeLoadSlicesP threads (Checked arr) = rangeLoadSlicesP threads arr
    linearLoadSlicesP threads (Checked arr) = linearLoadSlicesP threads arr
    rangeLoadSlicesS (Checked arr) = rangeLoadSlicesS arr
    linearLoadSlicesS (Checked arr) = linearLoadSlicesS arr
    {-# INLINE rangeLoadSlicesP #-}
    {-# INLINE linearLoadSlicesP #-}
    {-# INLINE rangeLoadSlicesS #-}
    {-# INLINE linearLoadSlicesS #-}


instance Fusion r fr sh a b => Fusion (CHK r) (CHK fr) sh a b where
    fmapM f = Checked . fmapM f . unchecked
    fzipM fun arrs =
        let uncheckedArrs = V.map unchecked arrs
        in Checked (fzipM fun uncheckedArrs)
    {-# INLINE fmapM #-}
    {-# INLINE fzipM #-}

instance DefaultFusion r fr sh a b => DefaultFusion (CHK r) (CHK fr) sh a b


instance UTarget tr sh a => UTarget (CHK tr) sh a where
    write (Checked arr) sh =
        let ext = extent arr
        in if not (insideBlock (zero, ext) sh)
            then error $ printf
                            "Yarr! Writing: index %s is out of extent - %s"
                            (show sh) (show ext)
            else write arr sh

    linearWrite (Checked arr) i =
        let sz = size (extent arr)
        in if not (insideBlock (0, sz) i)
            then error $ printf "Yarr! Writing: linear index %d is out of size - %d" i sz
            else linearWrite arr i
    {-# INLINE write #-}
    {-# INLINE linearWrite #-}

instance Manifest r mr sh a => Manifest (CHK r) (CHK mr) sh a where
    new sh = fmap Checked (new sh)
    freeze (Checked marr) = fmap Checked (freeze marr)
    thaw (Checked arr) = fmap Checked (thaw arr)
    {-# INLINE new #-}
    {-# INLINE freeze #-}
    {-# INLINE thaw #-}

instance UVecTarget tr sh tslr v e => UVecTarget (CHK tr) sh (CHK tslr) v e