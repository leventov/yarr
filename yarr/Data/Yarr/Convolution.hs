
module Data.Yarr.Convolution (
    -- * Convoluted representation
    module Data.Yarr.Convolution.Repr,
    -- | There is also @Convoluted@ 'UArray' family constructor,
    -- which isn't presented in the docs because Haddock
    -- doesn't support associated family constructors.
    --
    -- See source of "Data.Yarr.Convolution.Repr" module.

    module Data.Yarr.Convolution.Eval,
    -- * Static stencils
    module Data.Yarr.Convolution.StaticStencils
) where

import Data.Yarr.Convolution.Repr
import Data.Yarr.Convolution.Eval
import Data.Yarr.Convolution.StaticStencils