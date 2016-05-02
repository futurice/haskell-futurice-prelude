{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts     #-}
module HasTests where

import Futurice.Prelude
import Futurice.Has
import Prelude ()

import Generics.SOP (I(..), NP(..))

import Test.Tasty
import Test.Tasty.QuickCheck

hasTests :: TestTree
hasTests = testGroup "Futurice.Has"
    [ testProperty "here"    $ hereProp
    , testProperty "there"   $ there2Prop
    , testProperty "there 2" $ there2Prop
    ]

hereProp :: Bool -> Property
hereProp b = b === getBool (I b :* Nil)

thereProp :: Bool -> Property
thereProp b = b ===  getBool (I (1 :: Int) :* I b :* Nil)

there2Prop :: Bool -> Property
there2Prop b = b === getBool (I (1 :: Int) :* I b :* I (0.1 :: Double) :* Nil)

getBool :: Has a Bool => a -> Bool
getBool = view field
