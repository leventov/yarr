
module Data.Yarr.Repr.Delayed (
    -- * Delayed source
    D,
    -- * Delayed target
    DT,
    -- | There are also @LinearDelayed@, @ShapeDelayed@ and @ShapeDelayedTarget@
    -- 'UArray' family constructors,
    -- which aren't presented in the docs because Haddock
    -- doesn't support associated family constructors.
    --
    -- See source of "Data.Yarr.Repr.Delayed" module.
    UArray(..),

    -- * Misc
    L, SH, fromFunction, delay, delayShaped,
) where

import Prelude as P
import Control.Monad

import Data.Yarr.Base as B
import Data.Yarr.Eval
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V

-- | Delayed representation is a wrapper for arbitrary indexing function.
--
-- @'UArray' D 'L' sh a@ instance holds linear getter (@(Int -> IO a)@),
-- and @'UArray' D 'SH' sh a@ - shaped, \"true\" @(sh -> IO a)@ index, respectively.
--
-- @D@elayed arrays are most common recipients for fusion operations.
data D

instance Shape sh => Regular D L sh a where

    data UArray D L sh a =
        LinearDelayed
            !sh           -- Extent
            (IO ())       -- Array touch
            (IO ())       -- Inherited force
            (Int -> IO a) -- Linear get

    extent (LinearDelayed sh _ _ _) = sh
    touchArray (LinearDelayed _ tch _ _) = tch
    force (LinearDelayed sh _ iforce _) =
        (sh `deepseq` return ()) >> iforce

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}
    {-# INLINE force #-}

instance Shape sh => NFData (UArray D L sh a) where
    rnf (LinearDelayed sh tch iforce lget) =
        sh `deepseq` tch `seq` iforce `seq` lget `seq` ()
    {-# INLINE rnf #-}

instance Shape sh => USource D L sh a where
    linearIndex (LinearDelayed _ _ _ lget) = lget
    {-# INLINE linearIndex #-}


instance (Shape sh, Vector v e) => VecRegular D D L sh v e where
    slices (LinearDelayed sh tch iforce lget) =
        V.generate
            (\i -> LinearDelayed sh tch iforce ((return . (V.! i)) <=< lget))
    {-# INLINE slices #-}

instance (Shape sh, Vector v e) => UVecSource D D L sh v e

instance Fusion r D L where
    fmapM f arr =
        LinearDelayed
            (extent arr) (touchArray arr) (force arr) (f <=< linearIndex arr)

    fzip2M f arr1 arr2 =
        let sh = intersect (vl_2 (extent arr1) (extent arr2))
            tch = touchArray arr1 >> touchArray arr2
            iforce = force arr1 >> force arr2

            {-# INLINE lget #-}
            lget i = do
                v1 <- linearIndex arr1 i
                v2 <- linearIndex arr2 i
                f v1 v2

        in LinearDelayed sh tch iforce lget

    fzip3M f arr1 arr2 arr3 =
        let sh = intersect (vl_3 (extent arr1) (extent arr2) (extent arr3))
            tch = touchArray arr1 >> touchArray arr2 >> touchArray arr3
            iforce = force arr1 >> force arr2 >> force arr3

            {-# INLINE lget #-}
            lget i = do
                v1 <- linearIndex arr1 i
                v2 <- linearIndex arr2 i
                v3 <- linearIndex arr3 i
                f v1 v2 v3

        in LinearDelayed sh tch iforce lget

    fzipM fun arrs =
        let shapes = V.map extent arrs
            sh = V.head shapes

            tch = V.mapM_ touchArray arrs

            iforce = V.mapM_ force arrs

            lgets = V.map linearIndex arrs
            {-# INLINE lget #-}
            lget i = do
                v <- V.mapM ($ i) lgets
                inspect v fun

        in if V.all (== sh) shapes
                then LinearDelayed sh tch iforce lget
                else error ("Yarr! All arrays in linear zip " ++
                            "must be of the same extent")

    {-# INLINE fmapM #-}
    {-# INLINE fzip2M #-}
    {-# INLINE fzip3M #-}
    {-# INLINE fzipM #-}

instance DefaultFusion D D L



instance Shape sh => Regular D SH sh a where

    data UArray D SH sh a =
        ShapeDelayed
            !sh           -- Extent
            (IO ())       -- Array touch
            (IO ())       -- Inherited force
            (sh -> IO a)  -- Shape get

    extent (ShapeDelayed sh _ _ _) = sh
    touchArray (ShapeDelayed _ tch _ _) = tch
    force (ShapeDelayed sh _ iforce _) = (sh `deepseq` return ()) >> iforce

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}
    {-# INLINE force #-}

instance Shape sh => NFData (UArray D SH sh a) where
    rnf (ShapeDelayed sh tch iforce get) =
        sh `deepseq` tch `seq` iforce `seq` get `seq` ()
    {-# INLINE rnf #-}

instance Shape sh => USource D SH sh a where
    index (ShapeDelayed _ _ _ get) = get
    {-# INLINE index #-}


instance (Shape sh, Vector v e) => VecRegular D D SH sh v e where
    slices (ShapeDelayed sh tch iforce get) =
        V.generate
            (\i -> ShapeDelayed sh tch iforce ((return . (V.! i)) <=< get))
    {-# INLINE slices #-}

instance (Shape sh, Vector v e) => UVecSource D D SH sh v e

instance Fusion r D SH where
    fmapM f arr =
        ShapeDelayed
            (extent arr) (touchArray arr) (force arr) (f <=< index arr)

    fzip2M f arr1 arr2 =
        let sh = intersect (vl_2 (extent arr1) (extent arr2))
            tch = touchArray arr1 >> touchArray arr2
            iforce = force arr1 >> force arr2

            {-# INLINE get #-}
            get sh = do
                v1 <- index arr1 sh
                v2 <- index arr2 sh
                f v1 v2

        in ShapeDelayed sh tch iforce get

    fzip3M f arr1 arr2 arr3 =
        let sh = intersect (vl_3 (extent arr1) (extent arr2) (extent arr3))
            tch = touchArray arr1 >> touchArray arr2 >> touchArray arr3
            iforce = force arr1 >> force arr2 >> force arr3

            {-# INLINE get #-}
            get sh = do
                v1 <- index arr1 sh
                v2 <- index arr2 sh
                v3 <- index arr3 sh
                f v1 v2 v3

        in ShapeDelayed sh tch iforce get

    fzipM fun arrs =
        let shapes = V.map extent arrs
            sh = intersect shapes

            tch = V.mapM_ touchArray arrs

            iforce = V.mapM_ force arrs

            gets = V.map index arrs
            {-# INLINE get #-}
            get sh = do
                v <- V.mapM ($ sh) gets
                inspect v fun

        in ShapeDelayed sh tch iforce get

    {-# INLINE fmapM #-}
    {-# INLINE fzip2M #-}
    {-# INLINE fzip3M #-}
    {-# INLINE fzipM #-}

instance DefaultFusion D D SH

-- | Load type preserving wrapping arbirtary array into 'D'elayed representation.
delay :: (USource r l sh a, USource D l sh a, Fusion r D l)
      => UArray r l sh a -> UArray D l sh a
{-# INLINE delay #-}
delay = B.fmap id

-- | Wrap indexing function into delayed representation.
-- 
-- Use this function carefully, don't implement through it something
-- that has specialized implementation in the library (mapping, zipping, etc).
--
-- Suitable to obtain arrays of constant element,
-- of indices (@fromFunction sh 'id'@), and so on.
fromFunction
    :: Shape sh
    => sh               -- ^ Extent of array
    -> (sh -> IO a)     -- ^ Indexing function
    -> UArray D SH sh a -- ^ Result array
{-# INLINE fromFunction #-}
fromFunction sh f = ShapeDelayed sh (return ()) (return ()) f

-- | Wraps @('index' arr)@ into Delayed representation. Normally you shouldn't need
-- to use this function. It may be dangerous for performance, because
-- preferred 'Data.Yarr.Eval.Load'ing type of source array is ignored.
delayShaped :: USource r l sh a => UArray r l sh a -> UArray D SH sh a
{-# INLINE delayShaped #-}
delayShaped arr =
    ShapeDelayed (extent arr) (touchArray arr) (force arr) (index arr)

-- | In opposite to 'D'elayed (source) Delayed Target holds abstract /writing/
-- function: @(sh -> a -> IO ())@. It may be used to perform arbitrarily tricky
-- things, because no one obliges you to indeed write
-- an element inside wrapped function.
data DT

instance Shape sh => Regular DT SH sh a where

    data UArray DT SH sh a =
        ShapeDelayedTarget
            !sh                -- Extent
            (IO ())            -- Array touch
            (IO ())            -- Inherited force
            (sh -> a -> IO ()) -- Shape write

    extent (ShapeDelayedTarget sh _ _ _) = sh
    touchArray (ShapeDelayedTarget _ tch _ _) = tch
    force (ShapeDelayedTarget sh _ iforce _) =
        (sh `deepseq` return ()) >> iforce

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}
    {-# INLINE force #-}

instance Shape sh => NFData (UArray DT SH sh a) where
    rnf (ShapeDelayedTarget sh tch iforce wr) =
        sh `deepseq` tch `seq` iforce `seq` wr `seq` ()
    {-# INLINE rnf #-}

instance Shape sh => UTarget DT SH sh a where
    write (ShapeDelayedTarget _ _ _ wr) = wr
    {-# INLINE write #-}
