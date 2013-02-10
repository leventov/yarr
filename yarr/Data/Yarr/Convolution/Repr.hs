
module Data.Yarr.Convolution.Repr (
    CV, CVL, UArray(..), justCenter,
) where

import Prelude as P
import Control.Monad

import Data.Yarr.Base
import Data.Yarr.Shape
import Data.Yarr.Repr.Delayed

import Data.Yarr.Utils.FixedVector as V

-- | Convolution fused representation internally keeps 2 element getters:
--
--  * slow /border get/, which checks every index from applied stencil
--    to lay inside extent of underlying (convolved) source array.
--
--  * fast /center get/, which don't worries about bound checks
--
-- and 'center' 'Block'.
data CV

-- | ConVolution 'Data.Yarr.Eval.Load' type is specialized to load convolved arrays.
--
-- It loads 'center' with 'centerGet' and borders outside the center with
-- 'borderGet' separately.
--
-- It is even able to distribute quite expensive border loads evenly between
-- available threads while parallel load.
--
-- /TODO:/ element-wise Loading convolved arrays isn't inlined propely
-- with unrolled 'Fill'ing ('unrolledFill', 'dim2BlockFill').
-- However, with simple 'fill' performance is OK.
--
-- For details see
-- <http://stackoverflow.com/questions/14748900/ghc-doesnt-perform-2-stage-partial-application-inlining>
data CVL

instance Shape sh => Regular CV CVL sh a where

    data UArray CV CVL sh a =
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

-- | Retreives fast center get from convolved array
-- and wraps it into 'D'elayed array.
--
-- Remember that array indexing in Yarr is always zero-based,
-- so indices in result array are shifted by top-level corner offset
-- of given convolved array.
justCenter :: Shape sh => UArray CV CVL sh a -> UArray D SH sh a
{-# INLINE justCenter #-}
justCenter (Convoluted sh tch iforce _ (tl, br) cget) =
    ShapeDelayed (tl `offset` br) tch iforce (cget . (`plus` tl)) 

instance Shape sh => NFData (UArray CV CVL sh a) where
    rnf (Convoluted sh tch iforce bget center cget) =
        sh `deepseq` tch `seq` iforce `seq`
            bget `seq` center `deepseq` cget `seq` ()
    {-# INLINE rnf #-}


instance Shape sh => USource CV CVL sh a where
    index (Convoluted _ _ _ bget center cget) sh =
        if insideBlock center sh
            then cget sh
            else bget sh

    {-# INLINE index #-}


instance Fusion CV CV CVL where
    fmapM f (Convoluted sh tch iforce bget center cget) =
        Convoluted sh tch iforce (f <=< bget) center (f <=< cget)

    fzip2M f arr1 arr2 =
        let sh = intersect (vl_2 (extent arr1) (extent arr2))
            ctr = intersectBlocks (vl_2 (center arr1) (center arr2))
            tch = touchArray arr1 >> touchArray arr2
            iforce = force arr1 >> force arr2

            {-# INLINE bget #-}
            bget sh = do
                v1 <- borderGet arr1 sh
                v2 <- borderGet arr2 sh
                f v1 v2

            {-# INLINE cget #-}
            cget sh = do
                v1 <- centerGet arr1 sh
                v2 <- centerGet arr2 sh
                f v1 v2

        in Convoluted sh tch iforce bget ctr cget

    fzip3M f arr1 arr2 arr3 =
        let sh = intersect (vl_3 (extent arr1) (extent arr2) (extent arr3))
            ctr = intersectBlocks (vl_3 (center arr1) (center arr2) (center arr3))
            tch = touchArray arr1 >> touchArray arr2 >> touchArray arr3
            iforce = force arr1 >> force arr2 >> force arr3

            {-# INLINE bget #-}
            bget sh = do
                v1 <- borderGet arr1 sh
                v2 <- borderGet arr2 sh
                v3 <- borderGet arr3 sh
                f v1 v2 v3

            {-# INLINE cget #-}
            cget sh = do
                v1 <- centerGet arr1 sh
                v2 <- centerGet arr2 sh
                v3 <- centerGet arr3 sh
                f v1 v2 v3

        in Convoluted sh tch iforce bget ctr cget

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
    {-# INLINE fzip2M #-}
    {-# INLINE fzip3M #-}
    {-# INLINE fzipM #-}

instance DefaultFusion CV CV CVL
