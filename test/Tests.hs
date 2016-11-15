{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main (main) where

import Futurice.Generics
import Futurice.Time
import Futurice.Prelude
import Prelude           ()

import Data.Fixed (Centi)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified Data.Csv as Csv
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import HasTests
import ReflectionTests

data T  = T
    { _tInt   :: !Int
    , _tChar  :: !Char
    , _tText  :: !Text
    , _tMaybe :: !(Maybe Bool)
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
instance Aeson.ToJSON T where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ tests
    , hasTests
    , reflectionTests
    , tryDeepTests
    , toMapOfTests
    , swapMapMapTests
    , timeTests
    ]

tests :: TestTree
tests = testGroup "Futurice.Generics"
    [ testCase "Arbitrary.shrink" $
        length (shrink $ T 2 'b' "foo" (Just True)) > 0 @?= True
    , testCase "DefaultOrdered" $
        Csv.headerOrder (undefined :: T) @?= V.fromList ["int", "char", "text", "maybe"]
    , testCase "ToJSON omits Nothing" $
          Aeson.toJSON (T 0 'a' "foo" Nothing) @?= Aeson.Object (HM.fromList
              [ ("int", Aeson.Number 0)
              , ("char", "a")
              , ("text", "foo")
              ])
    , testProperty "FromJSON . ToEncoding" aesonRoundtripT
    , testProperty "FromJSON . ToJSON" aesonRoundtripT'
    , testProperty "FromRecord . ToNamedRecord" cassavaRoundtripT
    ]

aesonRoundtripT :: T -> Property
aesonRoundtripT t = lhs === rhs
  where
    lhs = Aeson.eitherDecode (Aeson.encode t)
    rhs = Right t

aesonRoundtripT' :: T -> Property
aesonRoundtripT' t = lhs === rhs
  where
    lhs = Aeson.eitherDecode (Aeson.encode (Aeson.toJSON t))
    rhs = Right t

cassavaRoundtripT :: [T] -> Property
cassavaRoundtripT t = lhs === rhs
  where
    lhs = Csv.decode Csv.HasHeader (Csv.encodeDefaultOrderedByName t)
    rhs = Right (V.fromList t)

tryDeepTests :: TestTree
tryDeepTests = testGroup "tryDeep"
    [ testCase "Shallow" $ do
        x <- fmap toMaybe $ tryDeep $ error "Error" :: IO (Maybe [Int])
        x @?= Nothing
    , testCase "Deep" $ do
        x <- fmap toMaybe $ tryDeep $ return [ error "Error" ] :: IO (Maybe [Int])
        x @?= Nothing
    , testCase "Works" $ do
        x <- fmap toMaybe $ tryDeep $ return [ 1 ] :: IO (Maybe [Int])
        x @?= Just [ 1 ]
    ]
  where
    toMaybe = either (const Nothing) Just

toMapOfTests :: TestTree
toMapOfTests = testGroup "toMapOf"
    [ testProperty "toMapOf ifolded = id" $ toMapOf_ifolded_prop
    ]
  where
    toMapOf_ifolded_prop :: Map Int Char -> Property
    toMapOf_ifolded_prop m = m === toMapOf ifolded m

swapMapMapTests :: TestTree
swapMapMapTests = testGroup "swapMapMap"
    [ testProperty "length" $ lengthProp
    ]
  where
    lengthProp :: Map Int (Map Char Bool) -> Property
    lengthProp m = length2 m === length2 m'
      where
        length2 :: (Foldable f, Foldable f') => f (f' a) -> Sum Int
        length2 = foldMap (Sum . length)
        m'      = swapMapMap m

timeTests :: TestTree
timeTests = testGroup "Futurice.Time"
    [ testCase "conversions" $ do
        let a = fromNominalDiffTime 3600
        let b = 60 :: NDT 'Minutes Centi
        ndtConvert' a @?= b
    ]
