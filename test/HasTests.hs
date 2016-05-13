{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
module HasTests where

import Futurice.Prelude
import Prelude          ()

import Futurice.Has     (Has (..), In')
import Generics.SOP (I (..), NP (..))

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

-------------------------------------------------------------------------------
-- Check it compiles
-------------------------------------------------------------------------------

inExample1 :: ()
inExample1 = ex
  where
    ex :: In' Bool '[Bool, Int, Double] => ()
    ex = ()
