{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main (main) where

import Futurice.Generics
import Futurice.Prelude
import Prelude           ()

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Csv as Csv
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V

import HasTests

data T  = T
    { _tInt  :: !Int
    , _tChar :: !Char
    , _tText :: !Text
    }
    deriving (Eq, Show)

deriveGeneric ''T

instance Arbitrary T where
    arbitrary = sopArbitrary
    shrink = sopShrink

instance Csv.DefaultOrdered T where headerOrder = sopHeaderOrder
instance Csv.FromRecord T where parseRecord = sopParseRecord
instance Csv.ToNamedRecord T where toNamedRecord = sopToNamedRecord
instance Aeson.FromJSON T where parseJSON = sopParseJSON 
instance Aeson.ToJSON T where toJSON = sopToJSON

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ tests
    , hasTests
    ]

tests :: TestTree
tests = testGroup "Futurice.Generics"
    [ testCase "Arbitrary.shrink" $ 
        length (shrink $ T 2 'a' "foo") > 0 @?= True
    , testCase "DefaultOrdered" $
        Csv.headerOrder (undefined :: T) @?= V.fromList ["int", "char", "text"]
    , testProperty "FromJSON . ToJSON" aesonRoundtripT
    , testProperty "FromRecord . ToNamedRecord" cassavaRoundtripT
    ]

aesonRoundtripT :: T -> Property
aesonRoundtripT t = lhs === rhs
  where
    lhs = Aeson.eitherDecode (Aeson.encode t)
    rhs = Right t

cassavaRoundtripT :: [T] -> Property
cassavaRoundtripT t = lhs === rhs
  where
    lhs = Csv.decode Csv.HasHeader (Csv.encodeDefaultOrderedByName t)
    rhs = Right (V.fromList t)
