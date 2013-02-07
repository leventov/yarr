
module Data.Yarr.Repr.Delayed (
    D, UArray(LinearDelayed, ShapeDelayed),
) where

import Prelude as P
import Control.Monad

import Data.Yarr.Base as B
import Data.Yarr.Shape
import Data.Yarr.Utils.FixedVector as V


data D

instance Shape sh => Regular D L sh a where

    data UArray D L sh a =
        LinearDelayed
            !sh           -- Extent
            (IO ())       -- Touch
            (Int -> IO a) -- Linear get

    extent (LinearDelayed sh _ _) = sh
    touch (LinearDelayed _ tch _) = tch

    {-# INLINE extent #-}
    {-# INLINE touch #-}

instance Shape sh => NFData (UArray D L sh a) where
    rnf (LinearDelayed sh tch lget) = sh `deepseq` tch `seq` lget `seq` ()
    {-# INLINE rnf #-}

instance Shape sh => USource D L sh a where
    linearIndex (LinearDelayed _ _ lget) = lget
    {-# INLINE linearIndex #-}


instance (Shape sh, Vector v e) => VecRegular D D L sh v e where
    slices (LinearDelayed sh tch lget) =
        V.generate (\i -> LinearDelayed sh tch ((return . (V.! i)) <=< lget))
    {-# INLINE slices #-}

instance (Shape sh, Vector v e) => UVecSource D D L sh v e

instance USource r L sh a => Fusion r D L sh a b where
    fmapM f arr =
        LinearDelayed (extent arr) (touch arr) (f <=< linearIndex arr)

    fzipM fun arrs =
        let shapes = V.map extent arrs
            sh = V.head shapes

            tch = V.mapM_ touch arrs

            lgets = V.map linearIndex arrs
            {-# INLINE lget #-}
            lget i = do
                v <- V.mapM ($ i) lgets
                inspect v fun

        in if V.all (== sh) shapes
                then LinearDelayed sh tch lget
                else error ("Yarr! All arrays in linear zip " ++
                            "must be of the same extent")

    {-# INLINE fmapM #-}
    {-# INLINE fzipM #-}

instance Shape sh => DefaultFusion D D L sh a b



instance Shape sh => Regular D SH sh a where

    data UArray D SH sh a =
        ShapeDelayed
            !sh           -- Extent
            (IO ())       -- Touch
            (sh -> IO a)  -- Shape get

    extent (ShapeDelayed sh _ _) = sh
    touch (ShapeDelayed _ tch _) = tch

    {-# INLINE extent #-}
    {-# INLINE touch #-}

instance Shape sh => NFData (UArray D SH sh a) where
    rnf (ShapeDelayed sh tch get) = sh `deepseq` tch `seq` get `seq` ()
    {-# INLINE rnf #-}

instance Shape sh => USource D SH sh a where
    index (ShapeDelayed _ _ get) = get
    {-# INLINE index #-}


instance (Shape sh, Vector v e) => VecRegular D D SH sh v e where
    slices (ShapeDelayed sh tch get) =
        V.generate (\i -> ShapeDelayed sh tch ((return . (V.! i)) <=< get))
    {-# INLINE slices #-}

instance (Shape sh, Vector v e) => UVecSource D D SH sh v e

instance USource r SH sh a => Fusion r D SH sh a b where
    fmapM f arr =
        ShapeDelayed (extent arr) (touch arr) (f <=< index arr)

    fzipM fun arrs =
        let shapes = V.map extent arrs
            sh = intersect shapes

            tch = V.mapM_ touch arrs

            gets = V.map index arrs
            {-# INLINE get #-}
            get sh = do
                v <- V.mapM ($ sh) gets
                inspect v fun

        in ShapeDelayed sh tch get

    {-# INLINE fmapM #-}
    {-# INLINE fzipM #-}

instance Shape sh => DefaultFusion D D SH sh a b

