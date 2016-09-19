{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module ReflectionTests where

import Futurice.Prelude
import Prelude          ()

import Data.Typeable                (typeRep)
import Futurice.Reflection.TypeLits

import Test.Tasty
import Test.Tasty.QuickCheck

reflectionTests :: TestTree
reflectionTests = testGroup "Futurice.Reflection.TypeLits"
    [ testProperty "Works" worksProp
    , testProperty "Agrees on GHC 7.10" ghc710Prop
    ]

worksProp :: Property
worksProp = once $ show "foo" === show (reifyTypeableSymbol p $ typeRep p)
  where p = Proxy :: Proxy "foo"

-- This tests compiles only with base >=4.8
ghc710Prop :: Property
ghc710Prop = once $ typeRep p === reifyTypeableSymbol p (typeRep p)
  where p = Proxy :: Proxy "foo"
