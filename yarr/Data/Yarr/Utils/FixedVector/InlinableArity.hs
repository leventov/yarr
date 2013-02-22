{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Data.Yarr.Utils.FixedVector.InlinableArity where

import Language.Haskell.TH hiding (Arity)

import Data.Vector.Fixed (Dim(..), Arity(..), Fun(..), Vector(..), (!), VecList(..), convert)
import Data.Vector.Fixed.Internal (arity)

class Arity ar => InlinableArity ar where
    inlinableZipWith
        :: (Vector v a, Vector v b, Vector v c, Dim v ~ ar)
        => (a -> b -> c) -> v a -> v b -> v c

    inlinableMap
        :: (Vector v a, Vector v b, Dim v ~ ar)
        => (a -> b) -> v a -> v b


makeInlinableArityInstance arityType a = do
    let n = fromIntegral $ arity a
        cfNames = map (\i -> mkName ("cf_" ++ (show i))) [1..n]
        cfs = map varE cfNames
        
        fN = mkName "f"
        fP = varP fN
        f = varE fN

        asN = mkName "as"
        asP = varP asN
        as = varE asN

        bsN = mkName "bs"
        bsP = varP bsN
        bs = varE bsN

        construct' vs =
            [| convert $ VecList $(listE vs) |]

        zipF = funD'
            'inlinableZipWith
            [clause
                [fP, asP, bsP]
                (normalB $
                    letE (concat $ zipWith
                            (\i cfN ->
                                let ie = litE (integerL i)
                                    ix l = [| (!) |] `appE` l `appE` ie
                                in funD' cfN [clause [] (normalB $ f `appE` (ix as) `appE` (ix bs)) []])
                            [0..n-1]
                            cfNames)
                         (construct' cfs))
                []]

        mapF = funD'
            'inlinableMap
            [clause
                [fP, asP]
                (normalB $
                    letE (concat $ zipWith
                            (\i cfN ->
                                let ie = litE (integerL i)
                                    ix l = [| (!) |] `appE` l `appE` ie
                                in funD' cfN [clause [] (normalB $ f `appE` (ix as)) []])
                            [0..n-1]
                            cfNames)
                         (construct' cfs))
                []]

    inst <- instanceD (cxt []) ((conT ''InlinableArity) `appT` arityType) (zipF ++ mapF)
    return [inst]


funD' name cs =
    let fd = funD name cs
        inline = pragInlD name Inline ConLike AllPhases
    in [fd, inline]