
module Data.Yarr.Utils.VecTuple (
    VecTuple(..), makeVecTupleInstance
) where

import Language.Haskell.TH

import Data.Vector.Fixed (Dim(..), Arity(..), Fun(..), Vector(..))
import Data.Vector.Fixed.Internal (arity)


data family VecTuple n e

funD' name cs =
    let fd = funD name cs
        inline = pragInlD name Inline ConLike AllPhases
    in [fd, inline]

makeVecTupleInstance arityType a = do

    let n = arity a
        ns = show n
        mkN name = mkName $ name ++ "_" ++ ns
        
        vtConName = mkN "VT"
        vtCon = conE vtConName

        e = varT $ mkName "e"
        tupleType = foldl appT (tupleT n) $ replicate n e

    familyInst <-
        newtypeInstD
            (cxt [])
            ''VecTuple
            [arityType, e]
            (recC vtConName
                  [varStrictType
                    (mkN "toTuple")
                    (strictType notStrict tupleType)])
            []

    let vn = (conT ''VecTuple) `appT` arityType
        vt = vn `appT` e

    dimInst <- tySynInstD ''Dim [vn] arityType
        
    let as = [mkName $ "a" ++ (show i) | i <- [1..n]]
        pas = fmap varP as
        eas = fmap varE as

        constructF = funD'
            (mkName "construct")
            [clause
                []
                (normalB $ appE (conE 'Fun) $ parensE $
                    lamE pas (appE vtCon (tupE eas)))
                []]

        instP = conP vtConName [tupP pas]
        fn = mkName "f"
        inspectF = funD'
            (mkName "inspect")
            [clause
                [instP, conP 'Fun [varP fn]]
                (normalB $ foldl appE (varE fn) eas)
                []]

    vectorInst <-
        instanceD
            (cxt [])
            ((conT ''Vector) `appT` vn `appT` e)
            (constructF ++ inspectF)


    let selectNames =
            [mkName $ "sel_" ++ ns ++ "_" ++ (show i) | i <- [1..n]]
        makeSelect i = funD'
            (selectNames !! (i - 1))
            [clause [instP] (normalB $ eas !! (i - 1)) []]

    selectDs <- sequence $ concat $ map makeSelect [1..n]


    return $ [familyInst, dimInst, vectorInst] ++ selectDs

