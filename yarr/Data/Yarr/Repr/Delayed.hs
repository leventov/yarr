
module Data.Yarr.Repr.Delayed (
    D, delay, delayShaped, DT, delayShapedTarget,
    L, SH,
    UArray(LinearDelayed, ShapeDelayed, ShapeDelayedTarget),
) where

import Prelude as P
import Control.Monad

import Data.Yarr.Base as B
import Data.Yarr.Eval
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


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

instance USource r L sh a => Fusion r D L sh a b where
    fmapM f arr =
        LinearDelayed
            (extent arr) (touchArray arr) (force arr) (f <=< linearIndex arr)

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
    {-# INLINE fzipM #-}

instance Shape sh => DefaultFusion D D L sh a b



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

instance USource r SH sh a => Fusion r D SH sh a b where
    fmapM f arr =
        ShapeDelayed
            (extent arr) (touchArray arr) (force arr) (f <=< index arr)

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
    {-# INLINE fzipM #-}

instance Shape sh => DefaultFusion D D SH sh a b

delay :: (USource r l sh a, USource D l sh a, Fusion r D l sh a a)
      => UArray r l sh a -> UArray D l sh a
{-# INLINE delay #-}
delay = B.fmap id

delayShaped :: USource r l sh a => UArray r l sh a -> UArray D SH sh a
{-# INLINE delayShaped #-}
delayShaped arr =
    ShapeDelayed (extent arr) (touchArray arr) (force arr) (index arr)


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

delayShapedTarget :: UTarget r l sh a => UArray r l sh a -> UArray DT SH sh a
{-# INLINE delayShapedTarget #-}
delayShapedTarget tarr =
    ShapeDelayedTarget
        (extent tarr) (touchArray tarr) (force tarr) (write tarr)