{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import Control.Applicative (liftA2)
import Data.Type.Equality
import Futurice.Generics
import Futurice.Monoid     (Average (..))
import Futurice.Prelude
import Futurice.Time
import Prelude ()

import Data.Fixed (Centi)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Aeson          as Aeson
import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

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
    , monoidLaws (Proxy :: Proxy (Average ApproxDouble)) "Average"
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

monoidLaws
    :: forall m. (Eq m, Show m, Arbitrary m, Semigroup m, Monoid m)
    => Proxy m -> String -> TestTree
monoidLaws _ n = testGroup (n <> " monoid laws")
    [ testProperty "left identity" leftIdentity
    , testProperty "right identity" rightIdentity
    , testProperty "associativity" associativity
    ]
  where
    leftIdentity :: m -> Property
    leftIdentity m = mempty <> m === m

    rightIdentity :: m -> Property
    rightIdentity m = m <> mempty === m

    associativity :: m -> m -> m -> Property
    associativity a b c = (a <> b) <> c === a <> (b <> c)

type ApproxDouble = Approx Double

newtype Approx a = Approx a
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Approx where
    pure = Approx
    Approx f <*> Approx x = Approx (f x)
    _ *> x = x

instance (Fractional a, Ord a) => Eq (Approx a) where
    Approx a == Approx b = abs (a - b) < 1e7 -- TODO: write better

instance Arbitrary a => Arbitrary (Approx a) where
    arbitrary = pure <$> arbitrary
    shrink = traverse shrink

instance Num a =>Num (Approx a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    (-) = liftA2 (-)

instance Fractional a => Fractional (Approx a) where
    fromRational = pure . fromRational
    recip = fmap recip
    (/) = liftA2 (/)

-------------------------------------------------------------------------------
-- :~: instance
-------------------------------------------------------------------------------

-- | NFData (a :~: b)
_proof :: ()
_proof = rnf (Refl :: () :~: ())
