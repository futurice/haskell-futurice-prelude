{-# LANGUAGE DataKinds, KindSignatures, GADTs, PolyKinds, TypeFamilies, TypeOperators #-}
-- | Peano numbers.
module Futurice.Peano (
    Peano(..),
    SPeano(..),
    SPeanoI(..),
    Index,
    ) where

import Data.Proxy (Proxy (..))

data Peano = PZ | PS Peano

data SPeano (p :: Peano) where
    SPZ :: SPeano 'PZ
    SPS :: SPeano p -> SPeano ('PS p)

class SPeanoI p where
    singPeano :: Proxy p -> SPeano p
instance SPeanoI 'PZ where singPeano _ = SPZ
instance SPeanoI p => SPeanoI ('PS p) where
    singPeano _ = SPS $ singPeano Proxy 

type family Index (x :: k) (xs :: [k]) :: Peano where
    Index x (x ': xs) = 'PZ
    Index x (y ': ys) = 'PS (Index x ys)
