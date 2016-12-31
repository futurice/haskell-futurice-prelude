module Futurice.Monoid where

import Prelude ()
import Futurice.Prelude
import Test.QuickCheck (Arbitrary (..))

-- | Numerically stable average. Weighted average to be precise.
data Average a = Average { _samples :: !a, getAverage :: !a }
  deriving (Eq, Show)

instance (Eq a, Fractional a) => Semigroup (Average a) where
    a@(Average n x) <> b@(Average n' x')
        | n == 0    = b
        | n' == 0   = a
        | otherwise = Average m y
      where
        m = n + n'
        y = (n * x + n' * x') / m

instance (Eq a, Fractional a) => Monoid (Average a) where
    mempty = Average 0 0
    mappend = (<>)

instance (Arbitrary a, Num a) => Arbitrary (Average a) where
    arbitrary = mk <$> arbitrary <*> arbitrary
      where
        mk s a = Average (abs s) a

    -- TOOD: write shrink. Let's do when there is shrinkMap
