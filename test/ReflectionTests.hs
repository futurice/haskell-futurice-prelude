{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}
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
#if MIN_VERSION_base(4,8,0)
    , testProperty "Agrees on GHC 7.10" ghc710Prop
#endif
    ]

worksProp :: Property
worksProp = once $ show "foo" === show (reifySymbolTypeable p $ typeRep p)
  where p = Proxy :: Proxy "foo"

#if MIN_VERSION_base(4,8,0)
ghc710Prop :: Property
ghc710Prop = once $ typeRep p === reifySymbolTypeable p (typeRep p)
  where p = Proxy :: Proxy "foo"
#endif
