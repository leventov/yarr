
module Data.Yarr.Convolution.Repr where

import Prelude as P
import Control.Monad

import Data.Yarr.Base
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed

import Data.Yarr.Utils.FixedVector as V


data CV

instance Shape sh => Regular CV CV sh a where

    data UArray CV CV sh a =
        Convoluted {
            getExtent      :: !sh,
            getTouch       :: IO (),
            inheritedForce :: IO (),
            borderGet      :: sh -> IO a,
            center         :: !(sh, sh),
            centerGet      :: sh -> IO a
        }

    extent = getExtent
    touchArray = getTouch
    force (Convoluted sh _ iforce _ center _) = do
        sh `deepseq` return ()
        center `deepseq` return ()
        iforce

    {-# INLINE extent #-}
    {-# INLINE touchArray #-}

justCenter :: Shape sh => UArray CV CV sh a -> UArray D SH sh a
{-# INLINE justCenter #-}
justCenter (Convoluted sh tch iforce _ (tl, br) cget) =
    ShapeDelayed (tl `offset` br) tch iforce (cget . (`plus` tl)) 

instance Shape sh => NFData (UArray CV CV sh a) where
    rnf (Convoluted sh tch iforce bget center cget) =
        sh `deepseq` tch `seq` iforce `seq`
            bget `seq` center `deepseq` cget `seq` ()
    {-# INLINE rnf #-}


instance Shape sh => USource CV CV sh a where
    index (Convoluted _ _ _ bget center cget) sh =
        if insideBlock center sh
            then cget sh
            else bget sh

    {-# INLINE index #-}


instance Shape sh => Fusion CV CV CV sh a b where
    fmapM f (Convoluted sh tch iforce bget center cget) =
        Convoluted sh tch iforce (f <=< bget) center (f <=< cget)

    fzipM fun arrs =
        let sh = intersect $ V.map extent arrs

            ctr = intersectBlocks $ V.map center arrs

            tch = V.mapM_ touchArray arrs

            iforce = V.mapM_ force arrs

            bgets = V.map borderGet arrs
            {-# INLINE bget #-}
            bget sh = do
                v <- V.mapM ($ sh) bgets
                inspect v fun

            cgets = V.map centerGet arrs
            {-# INLINE cget #-}
            cget sh = do
                v <- V.mapM ($ sh) cgets
                inspect v fun

        in Convoluted sh tch iforce bget ctr cget

    {-# INLINE fmapM #-}
    {-# INLINE fzipM #-}

instance Shape sh => DefaultFusion CV CV CV sh a b
