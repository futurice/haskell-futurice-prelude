{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main (main) where

import Futurice.Generics
import Futurice.Prelude
import Prelude           ()

import Data.Csv (DefaultOrdered (..))

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as V

data T  = T
    { _tInt  :: !Int
    , _tBool :: !Bool
    , _tText :: !Text
    }
    deriving (Eq, Show)

deriveGeneric ''T

instance Arbitrary T where
    arbitrary = sopArbitrary
    shrink = sopShrink

instance DefaultOrdered T where headerOrder = sopHeaderOrder

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Futurice.Generics"
    [ testCase "Arbitrary.shrink" $ 
        length (shrink $ T 2 True "foo") > 0 @?= True
    , testCase "DefaultOrdered" $
        headerOrder (undefined :: T) @?= V.fromList ["int", "bool", "text"]
    ]
