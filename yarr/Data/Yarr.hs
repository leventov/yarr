
{- | /Type system intro:/

    'Regular' is main type class in the library.
    Like @Source@ class in @repa@, it defines indexed type family: 'UArray'.
    Classes 'USource', for arrays which could be indexed, and 'UTarget',
    for mutable arrays, inherit from 'Regular'.

    As in @repa@, arrays in Yarr are type-indexed.
    'UArray' type family has 2 type indexes:

        * /representation index/ - the first type argument.

        * /load type index/ -
          the second argument of the type family. Pair of /load indexes/,
          from source and target array determines how arrays will be
          loaded one to another. Load index is mostly internal thing.
          See 'Load' class for details.

    Rest 2 'UArray' parameters generalize 'Shape' and element type.


    'VecRegular', 'UVecSource', 'UVecTarget' are counterparts for arrays
    of fixed-sized vectors.
    These classes have 6 arguments: repr type index, /slice repr type index/,
    load type index, shape, vector type, vector element.
    
    /Note:/ in the docs \"vector\" always stands for
    fixed-size vector. Don't confuse with vector from @vector@ library.

    As in @repa@, there are several kinds of representations:

        * 'Manifest' representations: 'F'oreign and 'Data.Yarr.Repr.Boxed.B'oxed
          with 'Data.Yarr.Repr.Boxed.MB' (Mutable Boxed).
          The difference between 'Manifest' and 'UTarget' arrays is that
          'Manifest' arrays could be created (see 'new' function).
          For example, 'FS' (Foreign Slice) is a slice representation for 'F'.
          FS-arrays are mutable, but you can't create a slice,
          you should firstly allocate entire 'F' array.

        * /Delayed/, or /fused/ representations: 'D'elayed
          and 'Data.Yarr.Convolution.Repr.CV' (ConVoluted).
          Arrays of these types aren't really exist in memory.
          Finally they should be loaded to manifest arrays.

        * /View/ representations: 'DT' (Delayed Target) and 'FS'.
          Useful for advanced hand-controlled flow operations.

        * /Meta/ representations: 'SE'parate
          and 'Debug.Yarr.CHK' (CHecKed).
          Thery are parameterized with another representation index.
          Arrays of meta types play almost like their prototypes.
          'SE' glues several arrays
          into one array of vectors (array types with 'SE' index are
          always instances of 'VecRegular' class).
          'Debug.Yarr.CHK' is useful for debugging,
          it raises error on illegal indexing attempt.
          By default indexing is unchecked.

    
    /Representation choice:/

    'F'oreign is the main manifest representation.
    \"Unboxed\" arrays of tuples from @repa@ and @vector@ libraries
    may be emulated by @('SE' 'F')@ type index,
    but keep in mind that they are usually slower than vanilla foreign arrays,
    because the latter are memory-local.

    /How to load array into memory:/

    Currently there is only one option \"out of the box\" - to load image :)
    See "Data.Yarr.IO.Image" module in @yarr-image-io@ package.

    /How to map and zip arrays:/

    See 'DefaultFusion' class and functions in "Data.Yarr.Flow" module.

    Example:

    @let delayedVecLengths = 'Data.Yarr.Flow.zipElems' (\x y -> sqrt (x * x + y * y)) vecs@

    /How to compute an array:/
    
    See 'Load' class and its counterpart 'VecLoad', and 'compute' function.

    Typical use:

    @vecLengths <- 'Data.Yarr.Eval.compute' ('Data.Yarr.Eval.loadP' 'Data.Yarr.Shape.fill' 'Data.Yarr.Eval.caps') delayedVecLengths@

    [@Working examples@] <https://github.com/leventov/yarr/tree/master/tests>

    /How to write fast program:/

        1. Read corresponding section in @repa@ guide:
           <http://hackage.haskell.org/packages/archive/repa/3.2.3.1/doc/html/Data-Array-Repa.html>

        2. Write @INLINE@ pragmas to all functions, including curried shortcuts.
           For example in such case: @let {myIndex = 'index' arr} in ...@
           you should write: @let {\{\-\# INLINE myIndex \#\-\};@
           @myIndex = 'index' arr} in ...@

        3. Although the library is highly generalized, target programs
           should be as as precise in types as possible.
           Don't neglect writing signatures for functions.

        4. You shouldn't be very keen on bang patterns.
           They are more likey to harm than improve performance.
           However, in 95% of cases GHC ignores them.

        5. Compilation flags:
           @-Odph -rtsopts -threaded -fno-liberate-case -funbox-strict-fields@
           @-fexpose-all-unfoldings -funfolding-keeness-factor1000@
           @-fsimpl-tick-factor=500 -fllvm -optlo-O3@.


    /Abbreviations across the library:/

    In names:

        * @U-@, @u-@, @unsafe-@ prefixes mean that:
          a) function parameters must conform special
          statically unchecked conditions, or b) it isn't OK just to call the function,
          you must do something else, call another function.
          All functions in type classes with @U-@ prefix
          ('USource', 'UTarget') are unsafe.

        * @d-@ prefix stands for \"default\". Typically function
          with @d-@ prefix is carried version of the one without prefix.

        * @f-@ prefix means \"fused\". Used for functions from 'Data.Yarr.Base.Fusion' class.

        * @-M@, as usual, is for monadic versions of functions.
          However, if there isn't non-monadic version
          (the most part of core functions), the suffix is omitted.

        * @-S@ and @-P@ are suffixes from @repa@, they indicate
          sequential and parallel versions of flow operation, respectively.

    In signatures:

        * @r@, @tr@, @mr@ - representation, target repr, manifest repr.
          For the first type index of 'UArray' family.

        * @slr@, @tslr@, @mslr@ - slice representation, respectively

        * @l@, @tl@ - load index, for the second argument of 'UArray'

        * @sh@ - array shape: 'Dim1', 'Dim2', or 'Dim3'

        * @v@, @v1@, @v2@ - 'Vector' type

        * @e@, @e2@ - vector element

        * @n@, @m@ - 'Arity' of vector
  
-}

module Data.Yarr (
    
    -- * Core type system
    module Data.Yarr.Base,

    -- ** Shapes
    Dim1, Dim2, Dim3,

    -- ** Fixed Vector
    Fun(..), Vector(..), VecList(VecList),
    N1, N2, N3, N4,


    -- * Dataflow (fusion operations)
    module Data.Yarr.Flow,

    
    -- ** 'Load'ing and computing arrays
    module Data.Yarr.Eval,
    

    -- * Common representations
    -- ** Foreign
    F, unsafeFromForeignPtr, toForeignPtr,
    
    -- ** Delayed
    D, UArray(LinearDelayed, ShapeDelayed), delay,

    -- ** Separate
    SE, fromSlices, unsafeMapSlices

) where

import Data.Yarr.Base hiding (Fusion(..))
import Data.Yarr.Eval
import Data.Yarr.Flow
import Data.Yarr.Shape
import Data.Yarr.Repr.Foreign
import Data.Yarr.Repr.Delayed
import Data.Yarr.Repr.Separate
import Data.Yarr.Utils.FixedVector as V