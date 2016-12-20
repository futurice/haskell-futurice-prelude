module Futurice.Monoid where

import Prelude ()
import Futurice.Prelude

-- | Numerically stable average. Weighted average to be precise.
data Average a = Average { _samples :: !a, getAverage :: !a }
  deriving (Eq, Show)

instance (Eq a, Fractional a) => Semigroup (Average a) where
    Average n x <> Average n' x' = if m == 0 then Average 0 0 else Average m y
      where
        m = n + n'
        y = (n * x + n' * x') / m

instance (Eq a, Fractional a) => Monoid (Average a) where
    mempty = Average 0 0
    mappend = (<>)
