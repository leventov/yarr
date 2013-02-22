{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

module Data.Yarr.Utils.FixedVector.InlinableArityInstances where

import Data.Vector.Fixed
import Data.Yarr.Utils.FixedVector.Arity
import Data.Yarr.Utils.FixedVector.InlinableArity

makeInlinableArityInstance [t| N1 |] n1
makeInlinableArityInstance [t| N2 |] n2
makeInlinableArityInstance [t| N3 |] n3
makeInlinableArityInstance [t| N4 |] n4
makeInlinableArityInstance [t| N5 |] n5
makeInlinableArityInstance [t| N6 |] n6
makeInlinableArityInstance [t| N7 |] n7
makeInlinableArityInstance [t| N8 |] n8